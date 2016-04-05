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
    test :: pid()     % testinternal state
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/0,
    start_iactor/1, stop_iactor/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_iactor(Name) ->
    gen_server:call(?MODULE, {start_iactor, Name}).

stop_iactor(Name) ->
    gen_server:call(?MODULE, {stop_iactor, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    % TODO: load info from database, so that the modules from before are added again
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, #state{}}. % 0 = initial state

handle_call({start_iactor, Name}, _From, State) ->
    % TODO: store info, which modules to start in database
    {reply, add_iactor_to_supervisor(Name), State};
handle_call({stop_iactor, Name}, _From, State) ->
    % TODO: update info of modules to start
    {reply, remove_iactor_from_supervisor(Name), State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, N) -> {noreply, N}.
terminate(_Reason, _N) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, N, _Extra) -> {ok, N}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% Dynamic IBO actor adding / removing
%%% * iactor = actor's supervisor which starts the actual IBO actor
%%% * private so that only the watchdog_server can access the pool directly!
%%%===================================================================
add_iactor_to_supervisor(SupervisorName) ->
    ActorSpec = #{id => SupervisorName,
        start => {SupervisorName, start_link, []},
        restart => transient,   % warning: when two nodes start the same global iactor -> restart crashes possible
        shutdown => 10000,  % = 10secs, time in milliseconds for shutdown before child is brutally killed
        type => supervisor,
        modules => dynamic},
    {ok, _Pid} = supervisor:start_child(iactor_sup, ActorSpec),
    ok.

remove_iactor_from_supervisor(SupervisorName) ->
    ok = supervisor:terminate_child(iactor_sup, SupervisorName),
    ok = supervisor:delete_child(iactor_sup, SupervisorName).