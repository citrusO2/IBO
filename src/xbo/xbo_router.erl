%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2015 11:47
%%%-------------------------------------------------------------------
-module(xbo_router).
-author("Florian").

-include("xbo_records.hrl").
-include("../xlib/xlib_state.hrl").

%% xbo_router internal state -----------------------------------------
-record(state, {
    name :: binary(),
    allowed_services :: nonempty_list(nonempty_string()),   % only services in the list are allowed to be used
    pids = [] :: list(pid())
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/1, stop/1, process_xbo/4, process_xbo/5, end_xbo/3, debug_xbo/4, xbo_childprocess/3]).

%% starts a new global router with the given name as the global name and allowed as a list of global names which the router is allowed to contact
-spec start_link(Args :: #{name => binary(), allowed => [binary()] }) ->  {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(Args) ->
    Name = maps:get(name, Args),
    gen_server:start_link({global, Name}, ?MODULE, Args, []).

%% stops a router by its name
-spec stop(Router :: binary()) -> ok.
stop(Router) ->
    gen_server:call({global, Router}, stop).

% processes a single XBO on the router without new StepData
-spec process_xbo(Router :: binary(), XBO :: #ibo_xbo{}, NewStepNr :: non_neg_integer(), Destination :: binary()) -> ok | {error, nonempty_string()}.
process_xbo(Router, XBO, NewStepNr, Destination) ->
    process_xbo(Router, XBO, NewStepNr, [], Destination).

% processes a single XBO on the router with new StepData
-spec process_xbo(Router :: binary(), XBO :: #ibo_xbo{}, NewStepNr :: non_neg_integer(), NewStepData :: #ibo_xbostepdata{}, Destination :: binary()) -> ok | {error, nonempty_string()}.
process_xbo(Router, XBO, NewStepNr, NewStepData, Destination) ->    % main function where IBOs get send to from other servers
    gen_server:call({global, Router}, {process_xbo, XBO, NewStepNr, NewStepData, Destination}).

% tells the router that the XBO fulfilled its task
-spec end_xbo(Router :: binary(), XBO :: #ibo_xbo{}, NewStepData :: #ibo_xbostepdata{}) -> ok.
end_xbo(Router, XBO, NewStepData) ->
    gen_server:call({global, Router}, {end_xbo, XBO, NewStepData}).

% tells the router that an error has occurred while handling the XBO
-spec debug_xbo(Router :: binary(), #xlib_state{}, Reason :: term(), From :: binary()) -> ok | {error, nonempty_string()}.
debug_xbo(Router, XlibState, Reason, From) ->
    gen_server:call({global, Router}, {debug_xbo, XlibState, Reason, From}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    Name = maps:get(name, Args),
    io:format("~p (~p) starting~n", [?MODULE, Name]),
    {ok, #state{allowed_services = maps:get(allowed, Args), name = Name}}. % initial state

handle_call({process_xbo, XBO, NewStepNr, NewStepData, Destination}, _From, State) ->
    % TODO: store the XBO, StepNr and maybe Time here (time is also stored in the stepdata), also maybe check XBO itself here
    % TODO: wrap in a try catch, add retry and send to deadletter instead
    case lists:any(fun(Elem) -> Elem =:= Destination end, State#state.allowed_services) of
        true ->
            NewXBO =
                if
                    NewStepData =:= [] -> XBO;
                    true -> XBO#ibo_xbo{stepdata = [NewStepData | XBO#ibo_xbo.stepdata]}
                end,
            Pid = spawn_link(?MODULE, xbo_childprocess, [Destination, NewXBO, NewStepNr]),
            NewState = save_pid(Pid, State),
            {reply, ok, NewState};  % received packet, but not yet forwarded
        false ->
            {reply, {error, "Destination is not allowed"}, State}
    end;
handle_call({end_xbo, _XBO, _NewStepData}, _From, State) ->
    {reply, ok, State};    % TODO: store information of XBO here and archive it
handle_call({debug_xbo, XlibState, Reason, From}, _From, State) ->
    ErrorSRV = get_first_error_server_from_xbo(XlibState#xlib_state.xbo),
    % the destination in the error server is set to the sender, because there was an error at the sender, after fixing the XBO the XBO should be send back to the sender by default
    {reply, error_server:process_xbo(ErrorSRV, XlibState, Reason, From), State};    % TODO: like process_xbo, put in a child-process
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'EXIT', Pid, normal}, State) ->
    io:format("xbo_router trapped EXIT signal normal~n"),
    NewState = remove_pid_from_state(Pid, State),
    {noreply, NewState};
handle_info({'EXIT', Pid, _Reason}, State) ->
    io:format("xbo_router trapped problematic EXIT signal -> TODO: handle the signal~n"),
    %TODO: error handling here, log locally because the childprocess should already send problematic ones to the error server
    NewState = remove_pid_from_state(Pid, State),
    {noreply, NewState}.
terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Executing XBO Childprocess
%%%===================================================================
xbo_childprocess(Destination, NewXBO, NewStepNr) ->
    io:format("sending XBO to ~p", [Destination]),
    case gen_server:call({global, Destination}, {process_xbo, NewXBO, NewStepNr}) of
        ok ->
            ok; % nothing to do anymore
        {error, Reason} ->
            error_server:process_xbo(get_first_error_server_from_xbo(NewXBO), NewXBO, NewStepNr, Reason, Destination)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
save_pid(Pid, State) ->
    State#state{pids = [Pid | State#state.pids]}.

remove_pid_from_state(Pid, State) ->
    State#state{pids = lists:dropwhile(fun(Element) -> Element =:= Pid end, State#state.pids)}.

get_first_error_server_from_xbo(XBO) ->
    lists:nth(1, XBO#ibo_xbo.error).