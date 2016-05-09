%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     Suite for application tests
%%% @end
%%% Created : 27. Apr 2016 13:34
%%%-------------------------------------------------------------------
-module(application_SUITE).
-author("Florian").

-include("template_ct_macros.hrl").

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([start_application_test/1, start_application_aftershutdown_test/1, start_complete_application_test/1]).

all() -> [start_application_test, start_application_aftershutdown_test, start_complete_application_test].

init_per_suite(Config) ->
    mnesia:stop(),
    ibo_app:install([node()]),
    ibo_app:start_dependencies(),
    Config.

end_per_suite(_Config) ->
    ibo_app:uninstall([node()]),
    ibo_app:stop_dependencies().

init_per_testcase(_, Config) ->
    ok = application:start(ibo),
    Config.

end_per_testcase(start_application_test, _Config) ->
    ok = application:stop(ibo),
    ok;
end_per_testcase(_, _Config) ->
    application:stop(ibo),
    ok = file:delete(deadletter_store),
    ok = file:delete(watchdog_configuration).

%%%===================================================================
%%% Application Tests
%%%===================================================================

%% the application should automatically start deadletter and watchdog
start_application_test(_Config) ->
    true = ct_helper:is_registered_local(ibo_sup),
    true = ct_helper:is_registered_local(watchdog_server),
    true = ct_helper:is_registered_local(deadletter_server),

    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    true = ct_helper:is_registered_global(?REPO_NAME),
    ok.

%% after stopping the application, the iactor started in the start_application_test should be started as well
start_application_aftershutdown_test(_Config) ->
    true = ct_helper:is_registered_local(watchdog_server),
    true = ct_helper:is_registered_local(deadletter_server),
    true = ct_helper:is_registered_global(?REPO_NAME),
    ok.

start_complete_application_test(_Config) ->
    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    ok = watchdog_server:start_iactor(directory_sup, ?DIRECTORY_ARGS),
    ok = watchdog_server:start_iactor(error_sup, ?ERROR_ARGS),
    ok = watchdog_server:start_iactor(web_sup, ?WEB_ARGS),
    ok = watchdog_server:start_iactor(xbo_router_sup, ?ROUTER_ARGS),
    ok = watchdog_server:start_iactor(box_sup, ?BOX_NAME),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
