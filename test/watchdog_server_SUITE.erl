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
-export([start_iactor_test/1, iactor_restart_test/1, iactor_restart_test2/1, iactor_restart_test3/1, iactor_doublestart_test/1, iactor_config_test/1, iactor_config_start_test/1, get_iactors_test/1, get_xactors_test/1, start_iactor_simple_test/1]).

-define(REPO_NAME, <<"REPO1">>).
-define(ROUTER_NAME, <<"ROUTER1">>).
-define(ERROR_SERVER_NAME, <<"ERRORSRV1">>).
-define(BOX_NAME, <<"BOX1">>).
-define(REPO_MANAGEGROUPS, [<<"ACL_SAVE_TEMPLATES">>]).
-define(REPO_ARGS, #{name =>?REPO_NAME, router => [?ROUTER_NAME], error => [?ERROR_SERVER_NAME], managegroups => ?REPO_MANAGEGROUPS }).
-define(ERROR_ARGS, #{name => ?ERROR_SERVER_NAME}).

all() -> [start_iactor_test, iactor_restart_test, iactor_restart_test2, iactor_restart_test3, iactor_doublestart_test, iactor_config_test, iactor_config_start_test, get_iactors_test, get_xactors_test, start_iactor_simple_test].

init_per_suite(Config) ->
    ok = mnesia:create_schema([node()]),
    mnesia:start(),
    Config.

end_per_suite(_Config) ->
    mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok.

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    start_watchdog_processes(Config).

end_per_testcase(iactor_config_start_test, _Config) ->
    ok = file:delete(watchdog_configuration); % watchdog processes are already not running (because they should crash)
end_per_testcase(_, Config) ->
    stop_watchdog_processes(Config),
    ok = file:delete(watchdog_configuration).    % persistend configuration for the watchdog need to be deleted, otherwise one tests affects the next one

%%%===================================================================
%%% User Tests
%%%===================================================================
start_iactor_test(_Config) ->   % it should be possible to start and stop an iactor dynamically
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    ct_helper:wait(),
    Ref = erlang:monitor(process,global:whereis_name(?REPO_NAME)),
    ok = watchdog_server:stop_iactor(?REPO_NAME),
    receive
        {'DOWN', Ref, process, _Pid, shutdown} ->
            ok
    after 1000 ->
        error(exit_timeout)
    end,
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    ct_helper:wait(),
    Ref2 = erlang:monitor(process,global:whereis_name(?REPO_NAME)),
    ok = watchdog_server:start_iactor(error_sup, ?ERROR_ARGS),
    ct_helper:wait(),
    Ref3 = erlang:monitor(process,global:whereis_name(?ERROR_SERVER_NAME)),
    ok = watchdog_server:stop_iactor(?ERROR_SERVER_NAME),
    receive
        {'DOWN', Ref3, process, _Pid3, shutdown} ->
            ok
    after 1000 ->
        error(exit_timeout)
    end,
    ok = watchdog_server:stop_iactor(?REPO_NAME),
    receive
        {'DOWN', Ref2, process, _Pid2, shutdown} ->
            ok
    after 1000 ->
        error(exit_timeout)
    end,
    ok.

iactor_restart_test(_Config) -> % when an iactor is stopped, it should be restarted automatically (by the supervisor)
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    RepoPid = global:whereis_name(?REPO_NAME),
    repo_server:stop(?REPO_NAME),

    ct_helper:waitms(1000),
    true = ct_helper:is_registered_global(?REPO_NAME),
    NewRepoPid = global:whereis_name(?REPO_NAME),
    true = RepoPid /= NewRepoPid,
    ok.

iactor_restart_test2(_Config) ->    % when an iactor is killed, it should be restarted as well
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    RepoPid = global:whereis_name(?REPO_NAME),
    exit(RepoPid, kill),

    ct_helper:wait(),
    true = ct_helper:is_registered_global(?REPO_NAME),
    NewRepoPid = global:whereis_name(?REPO_NAME),
    true = RepoPid /= NewRepoPid,
    ok.

iactor_restart_test3(_Config) ->    % when the watchdog dies, the pool has to be restarted and all the iactors in them
    %Sup = ?config(sup,_Config),
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    l_kill(watchdog_server),
    l_kill(watchdog_server),
    l_kill(watchdog_server),
    l_kill(watchdog_server),

    ct_helper:wait(1),
    true = ct_helper:is_registered_global(?REPO_NAME),
    ok.

iactor_doublestart_test(Config) ->
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    {error, _} = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),

    C1 = stop_watchdog_processes(Config),
    _C2 = start_watchdog_processes(C1),
    {error, _} = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),

    ok.

iactor_config_test(Config)->    % repo-server needs to be restarted again after shutdown of all the watchdog processes
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    true = ct_helper:is_registered_global(?REPO_NAME),

    C1 = stop_watchdog_processes(Config),
    false = ct_helper:is_registered_global(?REPO_NAME),

    _C2 = start_watchdog_processes(C1),
    true = ct_helper:is_registered_global(?REPO_NAME),
    ok.

iactor_config_start_test(Config) -> % watchdog_server should crash when it tries to startup and an iactor can't be started because it's already started by someone else
    true = ct_helper:is_registered_local(watchdog_server),
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    _C1 = stop_watchdog_processes(Config),
    false = ct_helper:is_registered_local(watchdog_server),
    repo_server:start_link(?REPO_ARGS),  % shouldn't be started directly, just for testing
    process_flag(trap_exit, true),
    {error, _} = watchdog_sup:start_link(), % whole watchdog-process should now crash when trying to start
    process_flag(trap_exit, false),
    false = ct_helper:is_registered_local(watchdog_server),
    repo_server:stop(?REPO_NAME),
    ok.

get_iactors_test(_Config) ->
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    ok = watchdog_server:start_iactor(error_sup, ?ERROR_ARGS),
    Iactors = watchdog_server:get_iactors(),
    true = lists:member( {?REPO_NAME, repo_sup, ?REPO_ARGS}, Iactors),
    true = lists:member( {?ERROR_SERVER_NAME, error_sup, ?ERROR_ARGS}, Iactors),
    ok.

get_xactors_test(_Config) ->
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    ok = watchdog_server:start_iactor(error_sup, ?ERROR_ARGS),
    [] = watchdog_server:get_xactors(),
    ok = watchdog_server:start_iactor(box_sup, ?BOX_NAME),
    Xactors = watchdog_server:get_xactors(),
    1 = length(Xactors),
    [Xactor] = Xactors,
    XlibInfo = box_server:xlib_info(),
    {?BOX_NAME, box_sup, _, XlibInfo} = Xactor,
    ok.

start_iactor_simple_test(_Config) -> % if the ARGS only consists of a map with the name of the iactor, then instead of a map a binary can be given
    ok = watchdog_server:start_iactor(error_sup, ?ERROR_SERVER_NAME),
    [Iactor] = watchdog_server:get_iactors(),
    Iactor = {?ERROR_SERVER_NAME, error_sup, ?ERROR_ARGS},
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

l_kill(RegisteredName) ->
    Pid = whereis(RegisteredName),
    exit(Pid, kill),
    ct_helper:waitms(100),
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