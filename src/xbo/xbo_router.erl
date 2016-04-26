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
-export([start_link/1, stop/1, process_xbo/3, process_xbo/4, strict_process_xbo/3, strict_process_xbo/4, super_strict_process_xbo/3, super_strict_process_xbo/4, end_xbo/2, strict_end_xbo/2, debug_xbo/3, strict_debug_xbo/3, super_strict_debug_xbo/3, xbo_childprocess/5]).

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
    process_xbo(XBO#ibo_xbo.router, XBO, NewStepNr, NewStepData, Destination, lenient).
    %gen_server:call({global, Router}, {process_xbo, XBO, NewStepNr, NewStepData, Destination}).

% like process_xbo, only that the packet is not sent to the deadletter and returns an error instead of forwarding it to the deadletter server
strict_process_xbo(XBO, NewStepNr, Destination) when is_record(XBO, ibo_xbo) ->
    strict_process_xbo(XBO, NewStepNr, [], Destination).

strict_process_xbo(XBO, NewStepNr, NewStepData, Destination) when is_record(XBO, ibo_xbo) ->
    process_xbo(XBO#ibo_xbo.router, XBO, NewStepNr, NewStepData, Destination, strict).

% like strict_process_xbo, but also waits till the message was received at the iactor till a reply is sent
super_strict_process_xbo(XBO, NewStepNr, Destination) when is_record(XBO, ibo_xbo) ->
    super_strict_process_xbo(XBO, NewStepNr, [], Destination).

super_strict_process_xbo(XBO, NewStepNr, NewStepData, Destination) when is_record(XBO, ibo_xbo) ->
    process_xbo(XBO#ibo_xbo.router, XBO, NewStepNr, NewStepData, Destination, superstrict).

% tells the router that the XBO fulfilled its task
-spec end_xbo(XBO :: #ibo_xbo{}, NewStepData :: #ibo_xbostepdata{}) -> ok.
end_xbo(XBO, NewStepData) when is_record(XBO, ibo_xbo) ->
    end_xbo(XBO#ibo_xbo.router, XBO, NewStepData, lenient).
    %gen_server:call({global, Router}, {end_xbo, XBO, NewStepData}).

% same as end_xbo, only that the packet is not sent to the deadletter and returns an error instead of forwarding it to the deadletter server
strict_end_xbo(XBO, NewStepData) when is_record(XBO, ibo_xbo) ->
    end_xbo(XBO#ibo_xbo.router, XBO, NewStepData, strict).

% tells the router that an error has occurred while handling the XBO
-spec debug_xbo(#xlib_state{}, Reason :: term(), From :: binary()) -> ok | {error, nonempty_string()}.
debug_xbo(XlibState = #xlib_state{xbo = #ibo_xbo{router = Router}}, Reason, From) ->
    debug_xbo(Router, XlibState, Reason, From, lenient).
    %gen_server:call({global, Router}, {debug_xbo, XlibState, Reason, From}).

strict_debug_xbo(XlibState = #xlib_state{xbo = #ibo_xbo{router = Router}}, Reason, From) ->
    debug_xbo(Router, XlibState, Reason, From, strict).
super_strict_debug_xbo(XlibState = #xlib_state{xbo = #ibo_xbo{router = Router}}, Reason, From) ->
    debug_xbo(Router, XlibState, Reason, From, superstrict).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    Name = maps:get(name, Args),
    io:format("~p (~p) starting~n", [?MODULE, Name]),
    {ok, #state{allowed_services = maps:get(allowed, Args), name = Name}}. % initial state

handle_call({process_xbo, XBO, NewStepNr, NewStepData, Destination, Type}, From, State) when Type =:= lenient ; Type =:= strict ->
    % TODO: store the XBO, StepNr and maybe Time here (time is also stored in the stepdata), also maybe check XBO itself here
    % TODO: wrap in a try catch, add retry and send to deadletter instead
    io:format("allowed services: ~p~n", [State#state.allowed_services]),
    case lists:any(fun(Elem) -> Elem =:= Destination end, State#state.allowed_services) of
        true ->
            NewXBO =
                if
                    NewStepData =:= [] -> XBO;
                    true -> XBO#ibo_xbo{stepdata = [NewStepData | XBO#ibo_xbo.stepdata]}
                end,
            Pid = spawn_link(?MODULE, xbo_childprocess, [Destination, NewXBO, NewStepNr, From, lenient]),
            NewState = save_pid(Pid, State),
            {reply, ok, NewState};  % received packet, but not yet forwarded
        false ->
            {reply, {error, "Destination is not allowed"}, State}   % TODO: change to ok, but forward it to error handler
    end;
handle_call({process_xbo, XBO, NewStepNr, NewStepData, Destination, superstrict}, From, State) ->
    % TODO: store the XBO, StepNr and maybe Time here (time is also stored in the stepdata), also maybe check XBO itself here
    % TODO: wrap in a try catch, add retry and send to deadletter instead
    case lists:any(fun(Elem) -> Elem =:= Destination end, State#state.allowed_services) of
        true ->
            NewXBO =
                if
                    NewStepData =:= [] -> XBO;
                    true -> XBO#ibo_xbo{stepdata = [NewStepData | XBO#ibo_xbo.stepdata]}
                end,
            Pid = spawn_link(?MODULE, xbo_childprocess, [Destination, NewXBO, NewStepNr, From, superstrict]),
            NewState = save_pid(Pid, State),
            {noreply, NewState};  % reply has to be made in the child-process
        false ->
            {reply, {error, "Destination is not allowed"}, State}   % TODO: change to ok, but forward it to error handler
    end;
handle_call({end_xbo, _XBO, _NewStepData}, _From, State) ->
    {reply, ok, State};    % TODO: store information of XBO here and archive it
handle_call({debug_xbo, XlibState, Reason, From, Type}, _From, State) when Type =:= lenient ; Type =:= strict -> %% if no error server is available sent to deadletter
    % send to error-server, fallback via deadletter
    {reply, send_to_error(XlibState, Reason, From), State};    % TODO: like process_xbo, put in a child-process
handle_call({debug_xbo, XlibState, Reason, From, superstrict}, _From, State) ->  %% message is not allowed to be send to deadletter
    % send to error_server, no fallback via deadletter!
    {reply, strict_send_to_error(XlibState, Reason, From), State};
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
xbo_childprocess(Destination, NewXBO, NewStepNr, _From, lenient)->
    %%io:format("sending XBO to ~p", [Destination]),
    try gen_server:call({global, Destination}, {process_xbo, NewXBO, NewStepNr}) of
        ok ->
            ok; % nothing to do anymore
        {error, Reason} -> % iactor did not accept the xbo
            ok = send_to_error(NewXBO, NewStepNr, Reason, Destination)
    catch
        exit:{noproc,_} ->
            ok = deadletter_server:dead_iactor(NewXBO, NewStepNr, Destination)
    end;
xbo_childprocess(Destination, NewXBO, NewStepNr, From, superstrict) ->  % has to return
    %%io:format("sending XBO to ~p", [Destination]),
    try gen_server:call({global, Destination}, {process_xbo, NewXBO, NewStepNr}) of
        ok ->
            gen_server:reply(From, ok),
            ok; % nothing to do anymore
        {error, Reason} -> % iactor did not accept the xbo
            Answer = strict_send_to_error(NewXBO, NewStepNr, Reason, Destination),
            gen_server:reply(From, Answer)
    catch
        exit:{noproc,_} ->
            gen_server:reply(From, {error, "cannot reach destination"})
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
save_pid(Pid, State) ->
    State#state{pids = [Pid | State#state.pids]}.

remove_pid_from_state(Pid, State) ->
    State#state{pids = lists:dropwhile(fun(Element) -> Element =:= Pid end, State#state.pids)}.

%%get_first_error_server_from_xbo(XBO) ->
%%    lists:nth(1, XBO#ibo_xbo.error).

% handles the choosing of the right router and retries using different routers
-spec process_xbo(RouterList :: [binary()] | [], XBO :: #ibo_xbo{}, NewStepNr :: non_neg_integer(), NewStepData :: #ibo_xbostepdata{}, Destination :: binary(), Type :: lenient | strict) -> ok | {error, nonempty_string()}.
process_xbo([], XBO, NewStepNr, NewStepData, Destination, lenient) -> % no router available, send to deadletter
    deadletter_server:dead_router(XBO, NewStepNr, NewStepData, Destination);
process_xbo([], _XBO, _NewStepNr, _NewStepData, _Destination, Type) when Type =:= superstrict ; Type =:= strict -> % no router available return error
    {error, "no router available"};
process_xbo([Router|OtherRouters], XBO, NewStepNr, NewStepData, Destination, Type) ->    % main function where IBOs get send to from other servers
    try gen_server:call({global, Router}, {process_xbo, XBO, NewStepNr, NewStepData, Destination, Type}) of
        ok -> ok;
        {error,Error} -> {error, Error}
    catch
        exit:{noproc,_} ->
            process_xbo(OtherRouters, XBO, NewStepNr, NewStepData, Destination, Type)
%%        Error:Msg ->
%%            io:format("Cannot process on other router (~p)(~p)", [Error, Msg])
    end.

end_xbo([], XBO, NewStepData, lenient) -> % no router available, send to deadletter
    deadletter_server:dead_router_end_xbo(XBO, NewStepData);
end_xbo([], _XBO, _NewStepData, strict) -> % no router available, return error
    {error, "no router available"};
end_xbo([Router|OtherRouters], XBO, NewStepData, Type) ->    % main function where IBOs get send to from other servers
    try gen_server:call({global, Router}, {end_xbo, XBO, NewStepData}) of
        ok -> ok;
        {error,Error} -> {error, Error}
    catch
        exit:{noproc,_} ->
            end_xbo(OtherRouters, XBO, NewStepData, Type)
%%        Error:Msg ->
%%            io:format("Cannot process on other router (~p)(~p)", [Error, Msg])
    end.

debug_xbo([], XlibState, Reason, From, lenient) -> % no router available, send to deadletter
    deadletter_server:dead_router_debug_xbo(XlibState, Reason, From);
debug_xbo([], _XlibState, _Reason, _From, Type) when Type =:= superstrict ; Type =:= strict -> % no router available, return error
    {error, "no router available"};
debug_xbo([Router|OtherRouters], XlibState, Reason, From, Type) ->    % main function where IBOs get send to from other servers
    try gen_server:call({global, Router}, {debug_xbo, XlibState, Reason, From, Type}) of
        ok -> ok;
        {error,Error} -> {error, Error}
    catch
        exit:{noproc,_} ->
            debug_xbo(OtherRouters, XlibState, Reason, From, Type)
%%        Error:Msg ->
%%            io:format("Cannot process on other router (~p)(~p)", [Error, Msg])
    end.

%%-----------------------------------------
%% internal send function for router only

send_to_error(XlibState, Reason, From) ->
    send_to_error(XlibState, Reason, From, lenient).

strict_send_to_error(XlibState, Reason, From) ->
    send_to_error(XlibState, Reason, From, strict).

strict_send_to_error(XBO, StepNr, Error, Destination) when is_record(XBO, ibo_xbo) ->
    strict_send_to_error(
        #xlib_state{xbo = XBO, current_stepdata = #ibo_xbostepdata{stepnr = StepNr}},
        Error,
        Destination
    ).

send_to_error(XBO, StepNr, Error, Destination) when is_record(XBO, ibo_xbo) ->
    send_to_error(
        #xlib_state{xbo = XBO, current_stepdata = #ibo_xbostepdata{stepnr = StepNr}},
        Error,
        Destination
    );
send_to_error(XlibState = #xlib_state{xbo = #ibo_xbo{error = ErrorServers}}, Reason, From, Type) -> %% init step
    send_to_error(ErrorServers, XlibState, Reason, From, Type).

send_to_error([], XlibState, Reason, From, lenient) ->
    deadletter_server:dead_error(XlibState, Reason, From);
send_to_error([], _XlibState, _Reason, _From, strict) ->
    {error, "no router available"};
send_to_error([ErrorServer|OtherErrorServers], XlibState, Reason, From, Type) ->
    try gen_server:call({global, ErrorServer}, {process_xbo, XlibState, Reason, From}) of
        ok -> ok;
        {error,Error} -> {error, Error}
    catch
        exit:{noproc,_} ->
            debug_xbo(OtherErrorServers, XlibState, Reason, From, Type)
    end.