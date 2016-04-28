%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     encapsulated send functions for xbo_router
%%% @end
%%% Created : 28. Apr 2016 15:10
%%%-------------------------------------------------------------------
-module(xbo_router_lib).
-author("Florian").

-include("xbo_records.hrl").
-include("../xlib/xlib_state.hrl").

%% API
-export([process_xbo/6,end_xbo/4, debug_xbo/5, send_to_error/3, strict_send_to_error/3, strict_send_to_error/4, send_xlibstate_to_error/5,
    send_xbo_to_destination/5]).

%%%===================================================================
%%% send functions executed by processes calling the xbo_router api
%%%===================================================================

% handles the choosing of the right router and retries using different routers
-spec process_xbo(RouterList :: [binary()] | [], XBO :: #ibo_xbo{}, NewStepNr :: non_neg_integer(), NewStepData :: #ibo_xbostepdata{}, Destination :: binary(), Type :: lenient | strict | superstrict) -> ok | {error, nonempty_string()}.
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

% handles the choosing of the right router and retries using different routers
-spec end_xbo(RouterList :: [binary()] | [], XBO :: #ibo_xbo{}, NewStepData :: #ibo_xbostepdata{}, Type :: lenient | strict | superstrict) -> ok | {error, nonempty_string()}.
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

% handles the choosing of the right error router
-spec debug_xbo(RouterList :: [binary()] | [], #xlib_state{}, Reason :: term(), From :: binary(), Type :: lenient | strict | superstrict) -> ok | {error, nonempty_string()}.
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

%%%===================================================================
%%% send functions used internally by the xbo_router in a separate thread
%%%===================================================================
send_xbo_to_destination(Destination, NewXBO, NewStepNr, _From, lenient)->
    try gen_server:call({global, Destination}, {process_xbo, NewXBO, NewStepNr}) of
        ok ->
            ok; % nothing to do anymore
        {error, Reason} -> % iactor did not accept the xbo
            ok = send_to_error(NewXBO, NewStepNr, Reason, Destination)
    catch
        exit:{noproc,_} ->
            ok = deadletter_server:dead_iactor(NewXBO, NewStepNr, Destination)
    end;
send_xbo_to_destination(Destination, NewXBO, NewStepNr, From, superstrict) ->  % has to return
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

%% send to error-server, if no error server is available sent to deadletter
send_xlibstate_to_error(XlibState, Reason, Sender, From, Type) when Type =:= lenient ; Type =:= strict ->
    Answer = send_to_error(XlibState, Reason, Sender),
    gen_server:reply(From, Answer);
%% send to error-server, if no error server is available an error message is given bag
send_xlibstate_to_error(XlibState, Reason, Sender, From, superstrict) ->
    Answer = strict_send_to_error(XlibState, Reason, Sender),
    gen_server:reply(From, Answer).

%%%===================================================================
%%% send functions used internally by the xbo_router
%%%===================================================================
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
            send_to_error(OtherErrorServers, XlibState, Reason, From, Type)
    end.