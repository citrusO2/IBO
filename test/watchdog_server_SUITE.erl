%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2016 19:30
%%%-------------------------------------------------------------------
-module(watchdog_server_SUITE).
-author("Florian").

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([start_iactor_test/1, iactor_restart_test/1, iactor_restart_test2/1, iactor_restart_test3/1, iactor_doublestart_test/1, iactor_config_test/1, iactor_config_start_test/1]).

all() -> [start_iactor_test, iactor_restart_test, iactor_restart_test2, iactor_restart_test3, iactor_doublestart_test, iactor_config_test, iactor_config_start_test ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    start_watchdog_processes(Config).

end_per_testcase(iactor_config_start_test, Config) ->
    Config;
    %dets:delete_all_objects(watchdog_configuration);    % watchdog processes are already not running (because they should crash)
end_per_testcase(_, Config) ->
    dets:delete_all_objects(watchdog_configuration),    % persistend configuration for the watchdog need to be deleted, otherwise one tests affects the next one
    stop_watchdog_processes(Config).

%%%===================================================================
%%% User Tests
%%%===================================================================
start_iactor_test(_Config) ->   % it should be possible to start and stop an iactor dynamically
    ok = watchdog_server:start_iactor(repo_sup),
    ct_helper:wait(),
    Ref = erlang:monitor(process,global:whereis_name(repo_server)),
    ok = watchdog_server:stop_iactor(repo_sup),
    receive
        {'DOWN', Ref, process, _Pid, shutdown} ->
            ok
    after 1000 ->
        error(exit_timeout)
    end,
    ok = watchdog_server:start_iactor(repo_sup),
    ct_helper:wait(),
    Ref2 = erlang:monitor(process,global:whereis_name(repo_server)),
    ok = watchdog_server:start_iactor(error_sup),
    ct_helper:wait(),
    Ref3 = erlang:monitor(process,global:whereis_name(error_server)),
    ok = watchdog_server:stop_iactor(error_sup),
    receive
        {'DOWN', Ref3, process, _Pid3, shutdown} ->
            ok
    after 1000 ->
        error(exit_timeout)
    end,
    ok = watchdog_server:stop_iactor(repo_sup),
    receive
        {'DOWN', Ref2, process, _Pid2, shutdown} ->
            ok
    after 1000 ->
        error(exit_timeout)
    end,
    ok.

iactor_restart_test(_Config) -> % when an iactor is stopped, it should be restarted automatically (by the supervisor)
    ok = watchdog_server:start_iactor(repo_sup),
    RepoPid = global:whereis_name(repo_server),
    repo_server:stop(),

    ct_helper:wait(),
    true = g_is_registered(repo_server),
    NewRepoPid = global:whereis_name(repo_server),
    true = RepoPid /= NewRepoPid,
    ok.

iactor_restart_test2(_Config) ->    % when an iactor is killed, it should be restarted as well
    ok = watchdog_server:start_iactor(repo_sup),
    RepoPid = global:whereis_name(repo_server),
    exit(RepoPid, kill),

    ct_helper:wait(),
    true = g_is_registered(repo_server),
    NewRepoPid = global:whereis_name(repo_server),
    true = RepoPid /= NewRepoPid,
    ok.

iactor_restart_test3(_Config) ->    % when the watchdog dies, the pool has to be restarted and all the iactors in them
    Sup = ?config(sup,_Config),
    ct_helper:print_var("children",supervisor:which_children(Sup)),

    ok = watchdog_server:start_iactor(repo_sup),

    l_kill(watchdog_server),
    l_kill(watchdog_server),
    l_kill(watchdog_server),
    l_kill(watchdog_server),

    ct_helper:wait(1),
    true = g_is_registered(repo_server),
    ok.

iactor_doublestart_test(Config) ->
    ok = watchdog_server:start_iactor(repo_sup),
    {error, _} = watchdog_server:start_iactor(repo_sup),

    C1 = stop_watchdog_processes(Config),
    _C2 = start_watchdog_processes(C1),
    {error, _} = watchdog_server:start_iactor(repo_sup),

    ok.

iactor_config_test(Config)->    % repo-server needs to be restarted again after shutdown of all the watchdog processes
    ok = watchdog_server:start_iactor(repo_sup),
    true = g_is_registered(repo_server),

    C1 = stop_watchdog_processes(Config),
    false = g_is_registered(repo_server),

    _C2 = start_watchdog_processes(C1),
    true = g_is_registered(repo_server),
    ok.

iactor_config_start_test(Config) -> % watchdog_server should crash when it tries to startup and an iactor can't be started because it's already started by someone else
    true = l_is_registered(watchdog_server),
    ok = watchdog_server:start_iactor(repo_sup),
    _C1 = stop_watchdog_processes(Config),
    false = l_is_registered(watchdog_server),
    repo_server:start_link({["xbo_router"], ["should_be_deadletter_server"],"repo1",1}),  % shouldn't be started directly, just for testing
    process_flag(trap_exit, true),
    {error, _} = watchdog_sup:start_link(), % whole watchdog-process should now crash when trying to start
    process_flag(trap_exit, false),
    false = l_is_registered(watchdog_server),
    repo_server:stop(),
%%    true = dets:is_dets_file(watchdog_configuration),
%%    {ok, _} = dets:open_file(watchdog_configuration),
%%    dets:delete_all_objects(watchdog_configuration),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
g_is_registered(Name) ->
    case global:whereis_name(Name) of
        undefined ->
            false;
        _Pid ->
            true
    end.

l_is_registered(Name) ->
    case whereis(Name) of
        undefined ->
            false;
        _Pid ->
            true
    end.

l_kill(RegisteredName) ->
    Pid = whereis(RegisteredName),
    exit(Pid, kill),
    ct_helper:waitms(200),
    ok.

stop_watchdog_processes(Config) ->
    Sup = ?config(sup,Config),
    process_flag(trap_exit, true),  % need to trap the exit of the supervisor, otherwise the test-process dies as well!
    Ref = erlang:monitor(process,Sup),
    erlang:exit(Sup,shutdown),  % stop the watchdog's supervisor and wait till everything is down, also see: http://stackoverflow.com/questions/21138442/stopping-an-erlang-supervisor
    receive
        {'DOWN', Ref, process, Sup, _Reason} ->
            ok
    after 1000 ->
        error(exit_timeout)
    end,
    process_flag(trap_exit, false),
    proplists:delete(sup, Config).

start_watchdog_processes(Config) ->
    {'ok', Pid} = watchdog_sup:start_link(),
    [{sup,Pid} | Config].