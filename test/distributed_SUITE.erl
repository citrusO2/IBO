%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Mai 2016 18:03
%%%-------------------------------------------------------------------
-module(distributed_SUITE).
-author("Florian").

-include("template_ct_macros.hrl").
-define(SLAVE_NODE, 'testslave').

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([start_slave_test/1, ibo_repo_router_box_test/1]).

all() -> [start_slave_test, ibo_repo_router_box_test].

init_per_suite(Config) ->
    mnesia:stop(),
    ibo_app:install([node()]),
    ibo_app:start_dependencies(),
    start_slave(?SLAVE_NODE, Config).

end_per_suite(_Config) ->
    ibo_app:uninstall([node()]),
    ibo_app:stop_dependencies(),
    stop_slave(?SLAVE_NODE),
    ok = file:delete(deadletter_store),
    ok = file:delete(watchdog_configuration).

init_per_testcase(_, Config) ->
    ok = application:start(ibo),
    Config.

end_per_testcase(_, _Config) ->
    ok = application:stop(ibo).
    %ok = file:delete(deadletter_store),
    %ok = file:delete(watchdog_configuration)

%%%===================================================================
%%% Distributed Tests
%%%===================================================================

%% master and slave nodes should both be running and the watchdog_server should be accessible
start_slave_test(Config) ->
    Slave = ?config(hostnode, Config),
    ct_helper:print_var("Slave", Slave),

    true = is_map(watchdog_server:connection_status()),
    true = is_map(watchdog_remote:connection_status(Slave)),
    ok.

%% start a router on the slave and start a template via router to the box
ibo_repo_router_box_test(Config) ->
    Slave = ?config(hostnode, Config),

    ok = watchdog_server:start_iactor(repo_sup, ?REPO_ARGS),
    ok = watchdog_server:start_iactor(directory_sup, ?DIRECTORY_ARGS),
    ok = watchdog_server:start_iactor(error_sup, ?ERROR_ARGS),
    ok = watchdog_server:start_iactor(web_sup, ?WEB_ARGS),
    ok = watchdog_server:start_iactor(box_sup, ?BOX_NAME),

    ok = watchdog_remote:start_iactor(Slave, xbo_router_sup, ?ROUTER_ARGS),

    Template = ?TEMPLATE_TESTTEMPLATE1,
    User = ?MARKETINGUSER,
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok = repo_server:store_template(?REPO_NAME, Template),
    ok = repo_server:start_template(?REPO_NAME, User, Template#ibo_repo_template.template),
    ct_helper:wait(),

    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_slave(Node, Config) ->
    ErlFlags =
        "-pa ../../ebin ",% ++ "-run ibo_app start_dependencies", % -name " ++ atom_to_list(Node),
    {ok, HostNode} = ct_slave:start(Node,
        [{kill_if_fail, true},
            {monitor_master, true},
            {init_timeout, 5},
            {startup_timeout, 5},
            {boot_timeout, 5},
            {env, [{"ERL_LIBS","../../deps"}]},
            %{startup_functions, [{ibo_app, start_dependencies, []}]},
            {erl_flags, ErlFlags}]),
    rpc:call(HostNode, ibo_app, install, [[HostNode]]),
    rpc:call(HostNode, ibo_app, start_dependencies, []),
    rpc:call(HostNode, application, start, [ibo]),
    ct_helper:print_var("Hostnode", HostNode),
    [{hostnode,HostNode} | Config].

stop_slave(Node) ->
    rpc:call(Node, application, stop, [ibo]),
    rpc:call(Node, ibo_app, stop_dependencies, [Node]),
    rpc:call(Node, ibo_app, uninstall, [[Node]]),
    {ok, _NodeName} = ct_slave:stop(Node),
    ok.

%%gethostname() ->
%%    Hostname = case net_kernel:longnames() of
%%       true->
%%           net_adm:localhost();
%%       _->
%%           {ok, Name}=inet:gethostname(),
%%           Name
%%    end,
%%    list_to_atom(Hostname).