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
-export([start_link/1, stop/1, process_xbo/3, process_xbo/4, strict_process_xbo/3, strict_process_xbo/4, super_strict_process_xbo/3, super_strict_process_xbo/4, end_xbo/2, strict_end_xbo/2, debug_xbo/3, strict_debug_xbo/3, super_strict_debug_xbo/3]).

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
-spec process_xbo(XBO :: #ibo_xbo{}, NewStepNr :: non_neg_integer(), Destination :: binary()) -> ok | {error, nonempty_string()}.
process_xbo(XBO, NewStepNr, Destination) ->
    process_xbo(XBO, NewStepNr, [], Destination).

% processes a single XBO on the router with new StepData
-spec process_xbo(XBO :: #ibo_xbo{}, NewStepNr :: non_neg_integer(), NewStepData :: #ibo_xbostepdata{}, Destination :: binary()) -> ok | {error, nonempty_string()}.
process_xbo(XBO, NewStepNr, NewStepData, Destination) when is_record(XBO, ibo_xbo) ->    % main function where IBOs get send to from other servers
    xbo_router_lib:process_xbo(XBO#ibo_xbo.router, XBO, NewStepNr, NewStepData, Destination, lenient).

% like process_xbo, only that the packet is not sent to the deadletter and returns an error instead of forwarding it to the deadletter server
strict_process_xbo(XBO, NewStepNr, Destination) when is_record(XBO, ibo_xbo) ->
    strict_process_xbo(XBO, NewStepNr, [], Destination).

strict_process_xbo(XBO, NewStepNr, NewStepData, Destination) when is_record(XBO, ibo_xbo) ->
    xbo_router_lib:process_xbo(XBO#ibo_xbo.router, XBO, NewStepNr, NewStepData, Destination, strict).

% like strict_process_xbo, but also waits till the message was received at the iactor till a reply is sent
super_strict_process_xbo(XBO, NewStepNr, Destination) when is_record(XBO, ibo_xbo) ->
    super_strict_process_xbo(XBO, NewStepNr, [], Destination).

super_strict_process_xbo(XBO, NewStepNr, NewStepData, Destination) when is_record(XBO, ibo_xbo) ->
    xbo_router_lib:process_xbo(XBO#ibo_xbo.router, XBO, NewStepNr, NewStepData, Destination, superstrict).

% tells the router that the XBO fulfilled its task
-spec end_xbo(XBO :: #ibo_xbo{}, NewStepData :: #ibo_xbostepdata{}) -> ok.
end_xbo(XBO, NewStepData) when is_record(XBO, ibo_xbo) ->
    xbo_router_lib:end_xbo(XBO#ibo_xbo.router, XBO, NewStepData, lenient).

% same as end_xbo, only that the packet is not sent to the deadletter and returns an error instead of forwarding it to the deadletter server
strict_end_xbo(XBO, NewStepData) when is_record(XBO, ibo_xbo) ->
    xbo_router_lib:end_xbo(XBO#ibo_xbo.router, XBO, NewStepData, strict).

% tells the router that an error has occurred while handling the XBO
-spec debug_xbo(#xlib_state{}, Reason :: term(), From :: binary()) -> ok | {error, nonempty_string()}.
debug_xbo(XlibState = #xlib_state{xbo = #ibo_xbo{router = Router}}, Reason, From) ->
    xbo_router_lib:debug_xbo(Router, XlibState, Reason, From, lenient).

% same as debug_xbo, only that the packet is not sent to the deadletter and returns an error instead of forwarding it to the deadletter server
strict_debug_xbo(XlibState = #xlib_state{xbo = #ibo_xbo{router = Router}}, Reason, From) ->
    xbo_router_lib:debug_xbo(Router, XlibState, Reason, From, strict).

% like strict_debug_xbo, but also waits till the message was received at the error server till a reply is sent
super_strict_debug_xbo(XlibState = #xlib_state{xbo = #ibo_xbo{router = Router}}, Reason, From) ->
    xbo_router_lib:debug_xbo(Router, XlibState, Reason, From, superstrict).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    Name = maps:get(name, Args),
    io:format("~p (~p) starting~n", [?MODULE, Name]),
    {ok, #state{allowed_services = maps:get(allowed, Args), name = Name}}. % initial state

handle_call({process_xbo, XBO, NewStepNr, [], Destination, Type}, From, State) -> % gets called for every processed xbo
    case lists:any(fun(Elem) -> Elem =:= Destination end, State#state.allowed_services) of
        true ->
            % TODO: store the XBO, StepNr and maybe Time here (time is also stored in the stepdata), also maybe check XBO itself here
            handle_call({process_xbo, XBO, NewStepNr, Destination, Type}, From, State);
        false ->
            {reply, {error, "Destination is not allowed"}, State}
    end;
handle_call({process_xbo, XBO, NewStepNr, NewStepData, Destination, Type}, From, State) ->
    handle_call({process_xbo,XBO#ibo_xbo{stepdata = [NewStepData | XBO#ibo_xbo.stepdata]}, NewStepNr, [], Destination, Type}, From, State);  % merge new stepdata to XBO
handle_call({process_xbo, XBO, NewStepNr, Destination, Type}, From, State) when Type =:= lenient ; Type =:= strict ->
    Pid = spawn_link(xbo_router_lib, send_xbo_to_destination, [Destination, XBO, NewStepNr, From, lenient]),
    NewState = save_pid(Pid, State),
    {reply, ok, NewState};  % received packet, but not yet forwarded
handle_call({process_xbo, XBO, NewStepNr, Destination, superstrict}, From, State) ->
    Pid = spawn_link(xbo_router_lib, send_xbo_to_destination, [Destination, XBO, NewStepNr, From, superstrict]),
    NewState = save_pid(Pid, State),
    {noreply, NewState};  % reply has to be made in the child-process
handle_call({end_xbo, _XBO, _NewStepData}, _From, State) ->
    {reply, ok, State};    % TODO: store information of XBO here and archive it
handle_call({debug_xbo, XlibState, Reason, Sender, Type}, From, State) ->
    Pid = spawn_link(xbo_router_lib, send_xlibstate_to_error, [XlibState, Reason, Sender, From, Type]),
    NewState = save_pid(Pid, State),
    {noreply, NewState};
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
%%% Internal functions
%%%===================================================================
save_pid(Pid, State) ->
    State#state{pids = [Pid | State#state.pids]}.

remove_pid_from_state(Pid, State) ->
    State#state{pids = lists:dropwhile(fun(Element) -> Element =:= Pid end, State#state.pids)}.
