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
    iactors :: list( {binary(), atom(), Args :: term() } )  ,   % list of actors to start at the beginning with their initialisation values, 1. element = global name, 2. element iactor type, 3. element Args
    filename = watchdog_configuration :: atom()
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/0, start_iactor/2, stop_iactor/1, get_iactors/0]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% starts an iactor with its supervisor name and the necessary Args for the iactor
%% the IactorType is the iactor's supervisor module name, e.g. repo_sup
-spec start_iactor(IactorType :: atom(), Args:: #{name => binary()}) -> ok | {error, term()};
                  (IactorType :: atom(), Name :: binary()) -> ok | {error, term()}.
start_iactor(IactorType, Args) when is_map(Args) ->
    gen_server:call(?MODULE, {start_iactor, IactorType, Args});
start_iactor(IactorType, Name) when is_binary(Name) -> % when no special ARGS are needed, the name can be given as a binary instead
    start_iactor(IactorType, #{name => Name}).

%% stops an iactor by its name
-spec stop_iactor(Name :: binary()) -> ok.
stop_iactor(Name) ->
    gen_server:call(?MODULE, {stop_iactor, Name}).

-spec get_iactors() -> [{Name :: binary(), IactorType :: atom(), Args :: map()}].
get_iactors() ->
    gen_server:call(?MODULE, get_iactors).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    S = #state{},

    % open settings from disk or create new settings on disk & start stored actors
    {ok, _} = dets:open_file(S#state.filename, []),
    Iactors = dets:foldl(fun({Name, IactorType, Args} = Iactor, AccIn) ->
        check_not_globally_registered(Name,watchdog_configuration_panic),   % configuration error, there shouldn't be another node with the same global iactors
        ok = add_iactor_to_supervisor(IactorType,Args),
        [Iactor|AccIn] end,[], S#state.filename),
    {ok, S#state{iactors = Iactors}}.

handle_call(get_iactors, _From, S) ->
    {reply, S#state.iactors,S};
handle_call({start_iactor, IactorType, Args}, _From, S) ->
    Name = maps:get(name, Args),
    case lists:keymember(Name, 1, S#state.iactors) of
        true ->
            {reply, {error, "watchdog already started iactor"}, S};
        false ->
            case global:whereis_name(Name) of
                undefined ->
                    try add_iactor_to_supervisor(IactorType, Args) of
                        ok ->
                            ok = dets:insert(S#state.filename,{Name, IactorType, Args}),
                            NewState = S#state{iactors = [{Name, IactorType,Args}|S#state.iactors]},
                            {reply, ok, NewState}
                    catch
                        error:ErrorType ->
                            {reply, {error, ErrorType}, S}
                    end;
                _Pid -> %
                    {reply, {error, "iactor already started globally"}, S}
            end
    end;
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
check_not_globally_registered(Name, IfErrorReason) ->
    case global:whereis_name(Name) of
        undefined ->
            ok;
        _Pid ->
            error(IfErrorReason)
    end.

%%%===================================================================
%%% Dynamic IBO actor adding / removing
%%% * iactor = actor's supervisor which starts the actual IBO actor
%%%===================================================================
add_iactor_to_supervisor(SupervisorName, Args) ->
    Name = maps:get(name, Args),
    ActorSpec = #{id => Name,
        start => {SupervisorName, start_link, [Args]},
        restart => transient,   % warning: when two nodes start the same global iactor -> restart crashes possible
        shutdown => 10000,  % = 10secs, time in milliseconds for shutdown before child is brutally killed
        type => supervisor,
        modules => dynamic},
    {ok, _Pid} = supervisor:start_child(iactor_sup, ActorSpec),
    ok.

remove_iactor_from_supervisor(Name) ->
    ok = supervisor:terminate_child(iactor_sup, Name),
    ok = supervisor:delete_child(iactor_sup, Name).
