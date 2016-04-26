%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2016 21:17
%%%-------------------------------------------------------------------
-module(deadletter_sendworker).
-author("Florian").

%% deadletter_sendworker internal state ------------------------------------------
-type deadletter_key() :: [{[binary()], [binary()], dead_router|dead_iactor|end_xbo|debug_xbo}].
-record(state, {
    filename :: atom(),
    key :: deadletter_key()
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/1]).

start_link(Args) when is_map(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    Filename = maps:get(filename, Args),
    Key = maps:get(key, Args),
    self() ! work,
    {ok, #state{filename = Filename, key = Key}}. % initial state
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

%server will call this function after initialisation
handle_info(work, S) ->
    ok = lists:foreach(
        fun({Key, Payload}) ->
            handle_case({Key, Payload}, S#state.filename)
        end, dets:lookup(S#state.filename, S#state.key)),    % too many xbos could become problematic in the future
    {stop, normal, S};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) ->
    dets:close(State#state.filename),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_case( {{Destination, _Router,dead_router}, {XBO, StepNr, StepData}} = Case, Filename) ->
    io:format("destination: ~p~n", [Destination]),
    ok = xbo_router:strict_process_xbo(XBO, StepNr, StepData, Destination), % just strict, because the package wasn't received by the router yet. Should the iactor die, the router will forward it to the local deadletter -> because the type will be dead_iactor, there is no delete/add race collision
    ok = dets:delete_object(Filename, Case);
handle_case( {{_Destination, _Router,dead_router_end_xbo}, {XBO, NewStepData}} = Case, Filename) ->
    ok = xbo_router:strict_end_xbo(XBO, NewStepData),   % strict, because otherwise we would send the packet from the deadletter_server to the same deadletter_server again
    ok = dets:delete_object(Filename, Case);
handle_case( {{_Destination, _Router,dead_router_debug_xbo}, {XlibState, Reason, From}} = Case, Filename) ->
    ok = xbo_router:strict_debug_xbo(XlibState, Reason, From),
    ok = dets:delete_object(Filename, Case);
handle_case( {{Destination, _Router,dead_iactor}, {XBO, StepNr}} = Case, Filename) ->
    ok = xbo_router:super_strict_process_xbo(XBO, StepNr, Destination),   % superstrict, so that the packet either reaches the iactor or causes an error, so that the packet doesn't reach the same deadletter_server again (to avoid delete/add race collision)
    ok = dets:delete_object(Filename, Case);
handle_case( {{_Destination, _Router,dead_error}, {XlibState, Reason, From}} = Case, Filename) ->
    ok = xbo_router:super_strict_debug_xbo(XlibState, Reason, From),
    ok = dets:delete_object(Filename, Case).
