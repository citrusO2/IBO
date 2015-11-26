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

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/1, stop/0, process_xbo/4]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).

process_xbo(XBO, NewStepNr, NewStepData, Destination) ->    % main function where IBOs get send to from other servers
    gen_server:call(?MODULE, {process_xbo, XBO, NewStepNr, NewStepData, Destination}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([AllowedServices]) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, #ibo_xborouter_state{allowed_services = AllowedServices}}. % initial state

handle_call({process_xbo, XBO, NewStepNr, NewStepData, Destination}, _From, State) ->
    % TODO: store the XBO, StepNr and maybe Time here (time is also stored in the stepdata), also maybe check XBO itself here
    % TODO: wrap in a try catch, add retry and send to deadletter instead
    case lists:any(fun(Elem) -> Elem =:= Destination end,State#ibo_xborouter_state.allowed_services) of
        true ->
            NewXBO = XBO#ibo_xbo{stepdata = [NewStepData|XBO#ibo_xbo.stepdata]},
            Res = apply(list_to_atom(Destination), process_xbo, [NewXBO, NewStepNr]),
            {reply, Res, State};
        false ->
            {reply, {error, "Destination is not allowed"}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================