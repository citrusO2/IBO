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

%% xbo_router internal state -----------------------------------------
-record(state, {
    allowed_services :: nonempty_list(nonempty_string()),   % only services in the list are allowed to be used
    pids = [] :: list(pid())
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/1, stop/0, process_xbo/4, end_xbo/2, debug_xbo/2, xbo_childprocess/3]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).

process_xbo(XBO, NewStepNr, NewStepData, Destination) ->    % main function where IBOs get send to from other servers
    gen_server:call(?MODULE, {process_xbo, XBO, NewStepNr, NewStepData, Destination}).

end_xbo(XBO, NewStepData) ->
    gen_server:call(?MODULE, {end_xbo, XBO, NewStepData}).

debug_xbo(XlibState,Reason) ->
    gen_server:call(?MODULE, {debug_xbo, XlibState, Reason}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([AllowedServices]) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, #state{allowed_services = AllowedServices}}. % initial state

handle_call({process_xbo, XBO, NewStepNr, NewStepData, Destination}, _From, State) ->
    % TODO: store the XBO, StepNr and maybe Time here (time is also stored in the stepdata), also maybe check XBO itself here
    % TODO: wrap in a try catch, add retry and send to deadletter instead
    case lists:any(fun(Elem) -> Elem =:= Destination end,State#state.allowed_services) of
        true ->
            NewXBO = XBO#ibo_xbo{stepdata = [NewStepData|XBO#ibo_xbo.stepdata]},
            Pid = spawn_link(?MODULE, xbo_childprocess, [Destination, NewXBO, NewStepNr]),
            NewState = save_pid(Pid, State),
            {reply, ok, NewState};  % received packet, but not yet forwarded
        false ->
            {reply, {error, "Destination is not allowed"}, State}
    end;
handle_call({end_xbo, _XBO, _NewStepData}, _From, State) ->
    {reply, ok, State};    % TODO: store information of XBO here and archive it
handle_call({debug_xbo, _XlibState, _Reason}, _From, State) ->
    {reply, ok, State};    % TODO: forward it to deadletter/error
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'EXIT', Pid, normal}, State) ->
    io:format("xbo_router trapped EXIT signal normal~n"),
    NewState = remove_pid(Pid, State),
    {noreply, NewState};
handle_info({'EXIT', Pid, _Reason}, State) ->
    io:format("xbo_router trapped problematic EXIT signal -> TODO: handle the signal~n"),
    %TODO: error handling here -> deadletter
    NewState = remove_pid(Pid, State),
    {noreply, NewState}.
terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Executing XBO Childprocess
%%%===================================================================
xbo_childprocess(Destination, NewXBO, NewStepNr) ->
    ok = apply(list_to_atom(Destination), process_xbo, [NewXBO, NewStepNr]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
save_pid(Pid, State) ->
    State#state{pids = [Pid | State#state.pids]}.

remove_pid(Pid, State) ->
    State#state{pids = lists:dropwhile(fun(Element) -> Element =:= Pid end, State#state.pids)}.