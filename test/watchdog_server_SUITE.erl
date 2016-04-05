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
-export([start_iactor_test/1, iactor_restart_test/1, iactor_restart_test2/1, iactor_restart_test3/1, iactor_config_test/1]).

all() -> [start_iactor_test, iactor_restart_test, iactor_restart_test2, iactor_restart_test3, iactor_config_test ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    start_watchdog_processes(Config).

end_per_testcase(_, Config) ->
    dets:delete_all_objects(watchdog_configuration),    % persistend configuration for the watchdog need to be deleted, otherwise one tests affects the next one
    stop_watchdog_processes(Config).

%%%===================================================================
%%% User Tests
%%%===================================================================
start_iactor_test(_Config) ->   % it should be possible to start and stop an iactor dynamically
    ok = watchdog_server:start_iactor(repo_sup),
    ct_helper:wait(),
    Ref = erlang:monitor(process,repo_server),
    ok = watchdog_server:stop_iactor(repo_sup),
    receive
        {'DOWN', Ref, process, _Pid, shutdown} ->
            ok
    after 1000 ->
        error(exit_timeout)
    end,
    ok.

iactor_restart_test(_Config) -> % when an iactor is stopped, it should be restarted automatically (by the supervisor)
    ok = watchdog_server:start_iactor(repo_sup),
    RepoPid = whereis(repo_server),
    repo_server:stop(),

    ct_helper:wait(),
    true = is_registered(repo_server),
    NewRepoPid = whereis(repo_server),
    RepoPid /= NewRepoPid,
    ok.

iactor_restart_test2(_Config) ->    % when an iactor is killed, it should be restarted as well
    ok = watchdog_server:start_iactor(repo_sup),
    RepoPid = whereis(repo_server),
    exit(RepoPid, kill),

    ct_helper:wait(),
    true = is_registered(repo_server),
    NewRepoPid = whereis(repo_server),
    RepoPid /= NewRepoPid,
    ok.

iactor_restart_test3(_Config) ->    % when the watchdog dies, the pool has to be restarted and all the iactors in them
    Sup = ?config(sup,_Config),
    ct_helper:print_var("children",supervisor:which_children(Sup)),

    ok = watchdog_server:start_iactor(repo_sup),

    kill(watchdog_server),
    kill(watchdog_server),
    kill(watchdog_server),
    kill(watchdog_server),

    ct_helper:wait(1),
    true = is_registered(repo_server),
    ok.

iactor_config_test(Config)->    % repo-server needs to be restarted again after shutdown of all the watchdog processes
    ok = watchdog_server:start_iactor(repo_sup),
    true = is_registered(repo_server),

    C1 = stop_watchdog_processes(Config),
    false = is_registered(repo_server),

    _C2 = start_watchdog_processes(C1),
    true = is_registered(repo_server),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
is_registered(Name) ->
    case whereis(Name) of
        undefined ->
            false;
        _Pid ->
            true
    end.

kill(RegisteredName) ->
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