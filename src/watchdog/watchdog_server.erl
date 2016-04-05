%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     The watchdog server is used to dynamically add and remove iactors
%%% @end
%%% Created : 04. Apr 2016 18:09
%%%-------------------------------------------------------------------
-module(watchdog_server).
-author("Florian").

%% watchdog_server internal state ----------------------------------------
-record(state, {
    iactors :: list( {atom(), Args :: term() } )  ,   % list of actors to start at the beginning with their initialisation values
    filename = watchdog_configuration :: atom()
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/0,
    start_iactor/1, start_iactor/2, stop_iactor/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_iactor(Name) ->
    start_iactor(Name, []).

start_iactor(Name, Args) ->
    gen_server:call(?MODULE, {start_iactor, Name, Args}).

stop_iactor(Name) ->
    gen_server:call(?MODULE, {stop_iactor, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    S = #state{},

    % open settings from disk or create new settings on disk & start stored actors
    {ok, _} = dets:open_file(S#state.filename, []),
    Iactors = dets:foldl(fun({Name, Args} = Iactor, AccIn) ->
        ok = add_iactor_to_supervisor(Name,Args),
        [Iactor|AccIn] end,[], S#state.filename),
    {ok, S#state{iactors = Iactors}}.

handle_call({start_iactor, Name, Args}, _From, S) ->
    ok = add_iactor_to_supervisor(Name, Args),  % try to start iactor first, otherwise a wrong call here would crash the server, which would then try to start the iactor again when restarting with the same errornous setings
    ok = dets:insert(S#state.filename,{Name, Args}),
    NewState = S#state{iactors = [{Name,Args}|S#state.iactors]},
    {reply, ok, NewState};
handle_call({stop_iactor, Name}, _From, S) ->
    ok = remove_iactor_from_supervisor(Name),
    ok = dets:delete(S#state.filename, Name),
    NewState = S#state{iactors = lists:keydelete(Name, 1, S#state.iactors)},
    {reply, ok, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, N) -> {noreply, N}.
terminate(_Reason, S) ->
    io:format("~p stopping~n", [?MODULE]),
    dets:close(S#state.filename),
    ok.
code_change(_OldVsn, N, _Extra) -> {ok, N}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Dynamic IBO actor adding / removing
%%% * iactor = actor's supervisor which starts the actual IBO actor
%%%===================================================================
add_iactor_to_supervisor(SupervisorName, Args) ->
    ActorSpec = #{id => SupervisorName,
        start => {SupervisorName, start_link, Args},
        restart => transient,   % warning: when two nodes start the same global iactor -> restart crashes possible
        shutdown => 10000,  % = 10secs, time in milliseconds for shutdown before child is brutally killed
        type => supervisor,
        modules => dynamic},
    {ok, _Pid} = supervisor:start_child(iactor_sup, ActorSpec),
    ok.

remove_iactor_from_supervisor(SupervisorName) ->
    ok = supervisor:terminate_child(iactor_sup, SupervisorName),
    ok = supervisor:delete_child(iactor_sup, SupervisorName).