%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     Server which stores XBOs with problems
%%%     TODO: Deadletter server should also redirect erroneous XBOs to an update server which tries to solve the XBOs problem
%%% @end
%%% Created : 04. Jän 2016 06:33
%%%-------------------------------------------------------------------
-module(deadletter_server).
-author("Florian").

-include("deadletter_records.hrl").
-include("../xlib/xlib_state.hrl").

%% deadletter internal state -----------------------------------------
-type deadletter_key() :: [{[binary()], [binary()], dead_router|dead_router_end_xbo|dead_router_debug_xbo|dead_iactor|dead_error}].
-type set( _ ) :: [any()].  % not really a list, but needs a type declaration

-record(state, {
    keys = [] :: set(deadletter_key()),   % list of keys to check
    filename = deadletter_store :: atom(),
    worker = [] :: [{deadletter_key(), reference()}],
    tickrate :: non_neg_integer()
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/1, stop/0, get_size/0, dead_router/4, dead_iactor/3, dead_router_end_xbo/2, dead_router_debug_xbo/3, dead_error/3]).

%% starts a local deadletter server, every node needs to have one, so that in case of e.g. netsplits the actors can continue working and are automatically resumed when the actors come back online
-spec start_link(Args :: map()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%% stops the deadletter server
-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

get_size() ->
    gen_server:call(?MODULE, size).

%%%================================
%%% Functions when the router couldn't be reached
%%% all dead_router functions indicate, that the XBO has not been received by the router yet
%%%================================

%% iactor tried to send the XBO to the router, but no router was available
dead_router(XBO = #ibo_xbo{router = Router}, StepNr, StepData, Destination) ->
    Key = {Destination, Router, dead_router},
    Payload = {XBO, StepNr, StepData},
    process_xbo(Key, Payload).

%% the iactor tried to end the XBO, but no router was available
dead_router_end_xbo(XBO = #ibo_xbo{router = Router}, NewStepData) ->
    Key = {[], Router, dead_router_end_xbo},
    Payload = {XBO, NewStepData},
    process_xbo(Key, Payload).

%% the iactor tried to send the xbo to the error server via the router, but no router was available
dead_router_debug_xbo(XlibState = #xlib_state{xbo = #ibo_xbo{router = Router, error = Error}}, Reason, From) ->
    Key = {Error, Router, dead_router_debug_xbo},
    Payload = {XlibState, Reason, From},
    process_xbo(Key, Payload).

%%%===============================
%%% Functions when the router could be reached, but the router couldn't forward to the destination
%%%===============================
%% xbo_router tried to send it to the iactor, but iactor was not available, so wait till the iactor comes back online to try again
dead_iactor(XBO = #ibo_xbo{router = Router}, StepNr, Destination) ->
    Key = {Destination, Router, dead_iactor},
    Payload = {XBO, StepNr},
    process_xbo(Key, Payload).

%% xbo_router tried to send the erroneous xbo to the error handler, but the error handler was not available
dead_error(XlibState = #xlib_state{xbo = #ibo_xbo{router = Router, error = Error}}, Reason, From) ->
    Key = {Error, Router, dead_error},
    Payload = {XlibState, Reason, From},
    process_xbo(Key, Payload).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    S = #state{tickrate = maps:get(tickrate, Args, 1000 * 60)},

    %% load stored destinations
    {ok, _} = dets:open_file(S#state.filename, [{type, bag}]),  % type bag so that more than one payload can be stored per key
    Keys = get_keys_from_file(S#state.filename),
    {ok, _} = timer:send_interval(S#state.tickrate, tick),  %% send a tick to itself, because updates to global need to be polled (no events available)
    {ok, S#state{keys = Keys}}.

handle_call({process_xbo, Key, Payload}, _From, S = #state{keys = Keys, filename = Filename}) ->
    NewKeys = sets:add_element(Key, Keys),
    Response = dets:insert(Filename, {Key, Payload}),
    {reply, Response, S#state{keys = NewKeys}};
handle_call(size, _From, S) ->
    {reply, dets:info(S#state.filename,size),S};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'DOWN', Ref, process, _Pid, _}, S = #state{worker = Worker, filename = Filename, keys = Keys}) ->
    NewState = case lists:keyfind(Ref, 2, Worker) of
                   {Key, Ref} ->
                       NewWorker = lists:keydelete(Ref, 2, Worker),
                       NewKeys = update_finished_keys(Key, Filename, Keys), % only updates keys when key is not in dets any more
                       S#state{worker = NewWorker, keys = NewKeys};
                   false -> % message is of no concern
                       S
               end,
    {noreply, NewState};
handle_info(tick, S = #state{worker = Worker, keys = Keys, filename = Filename}) ->
    KeysToProcess = get_keys_to_process(Keys, Worker),

    NewWorker = sets:fold(
        fun(Key, AccIn) ->
            {ok, Pid} = supervisor:start_child(deadletter_sendworker_sup, [#{filename => Filename, key => Key}]),
            Ref = erlang:monitor(process, Pid),
            [{Key, Ref} | AccIn]
        end,
        Worker, KeysToProcess),
    {noreply, S#state{worker = NewWorker}};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) ->
    io:format("~p stopping~n", [?MODULE]),
    dets:close(State#state.filename),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
process_xbo(Key, Payload) ->
    gen_server:call(?MODULE, {process_xbo, Key, Payload}).

%% filters out keys that cannot be processed at the moment
-spec get_keys_to_process(Keys :: set(deadletter_key()), Worker :: [{deadletter_key(), reference()}]) -> set(deadletter_key()).
get_keys_to_process(Keys, Worker) ->
    PrefilteredKeys = filter_keys_in_work(Keys, Worker),
    sets:fold(fun(Key, AccIn) ->
        case is_key_online(Key) of
            true -> sets:add_element(Key, AccIn);
            false -> AccIn
        end
              end, sets:new(), PrefilteredKeys).

is_key_online({Destinations, Router, _Type}) ->
    is_one_destination_online(Destinations) andalso is_one_router_online(Router).

% checks if at least one router is online from a list of routers
is_one_router_online([]) ->
    false;
is_one_router_online([Router|OtherRouters]) ->
    case is_destination_online(Router) of
        true -> true;
        false -> is_one_router_online(OtherRouters)
    end.

%checks if from a list of destination at least one is online, or returns true when the destination is empty to begin with

is_one_destination_online([]) -> % special case when no destination is needed/given
    true;
is_one_destination_online([Destination|OtherDestinations]) ->
    case is_destination_online(Destination) of
        true -> true;
        false ->
            if
                OtherDestinations =:= [] -> false;  % could be simplified if the special case would not need to return true
                true -> is_one_destination_online(OtherDestinations)
            end
    end;
is_one_destination_online(Destination) ->
    is_destination_online(Destination).

%% filters keys which are already currently worked on
-spec filter_keys_in_work(Keys :: set(deadletter_key()), Worker :: [{deadletter_key(), reference()}]) -> set(deadletter_key()).
filter_keys_in_work(Keys, Worker) ->
    sets:filter(
        fun(Key) ->
            lists:keymember(Key, 1, Worker) =:= false
        end
        , Keys).

is_destination_online(Destination) ->
    case global:whereis_name(Destination) of
        undefined -> false;
        _Pid -> true
    end.

get_keys_from_file(FileName) ->
    Set = sets:new(),
    case dets:first(FileName) of
        '$end_of_table' -> Set;
        Key ->
            get_keys_from_file(FileName, Key, sets:add_element(Key, Set))
    end.
get_keys_from_file(FileName, LastKey, Keys) ->
    case dets:next(FileName, LastKey) of
        '$end_of_table' -> Keys;
        Key -> get_keys_from_file(FileName, Key, sets:add_element(Key, Keys))
    end.

%% check if more XBOs are still assigned to the Key, otherwise remove the key from the set of keys
update_finished_keys(FinishedKey, Filename, Keys) ->
    case dets:member(Filename, FinishedKey) of
        true -> Keys; % there are still some new XBOs with this destination, cannot remove the destination yet!
        false -> sets:del_element(FinishedKey, Keys)
    end.

%% old functions, remove when feeling it's necessary
%%is_destination_in_destinations(Destination, Destinations) when is_list(Destinations) ->
%%    lists:keymember(Destination, 1, Destinations);
%%is_destination_in_destinations(Destination, State) when is_record(State, state) ->
%%    is_destination_in_destinations(Destination, State#state.destinations).

%%add_destination_to_state(Destination, State) ->
%%    State#state{destinations = [{Destination, waiting} | (State)#state.destinations]}.

%%%only remove destination if there are also no new XBOs for that destination saved
%%update_finished_destinations(FinishedDestination, Filename, Destinations) ->
%%    case dets:member(Filename, FinishedDestination) of
%%        true -> Destinations; % there are still some new XBOs with this destination, cannot remove the destination yet!
%%        false -> lists:delete(FinishedDestination, Destinations)
%%    end.

%%remove_destination_from_state(Destination, State) ->
%%    State#state{ destinations = lists:delete(Destination, State#state.destinations)}.

%%store_ibo(Filename, XBO, StepNr, StepData, Destination) ->
%%    dets:insert(Filename, {Destination, XBO, StepNr, StepData}).
%%get_waiting_online_destinations(Destinations, Worker) ->
%%    WaitingDestinations = filter_working_destinations(Destinations, Worker),
%%    lists:foldl(fun(Destination, AccIn) ->
%%        case is_destination_online(Destination) of
%%            true -> [Destination | AccIn];
%%            false -> AccIn
%%        end end, [], WaitingDestinations).
%%
%%filter_working_destinations(Destinations, Worker) ->
%%    lists:filter(
%%        fun(Destination) ->
%%            lists:keymember(Destination, 1, Worker) =:= false end
%%        , Destinations).