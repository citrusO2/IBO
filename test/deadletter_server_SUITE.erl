%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jän 2016 07:22
%%%-------------------------------------------------------------------
-module(deadletter_server_SUITE).
-author("Florian").

-include("../src/deadletter/deadletter_records.hrl").
-include("xbo_ct_macros.hrl").

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([start_deadletter_test/1, send_to_box_deadrouter_test/1, send_to_box_deadrouter_restart_test/1, send_to_deadbox_test/1, send_to_deadbox_strict_test/1, shutdown_restart_test/1]).

%% API
all() -> [start_deadletter_test, send_to_box_deadrouter_test, send_to_box_deadrouter_restart_test, send_to_deadbox_test,send_to_deadbox_strict_test, shutdown_restart_test].

init_per_suite(Config) ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    ct_helper:create_table_for_record(ibo_boxdata, record_info(fields, ibo_boxdata), Nodes),
    ct_helper:create_table_for_record(ibo_boxindex, record_info(fields, ibo_boxindex), Nodes),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:start(),
    mnesia:wait_for_tables([ibo_boxdata, ibo_boxindex], 5000),
    Config.

end_per_suite(_Config) ->
    Nodes = [node()],
    {atomic, ok} = mnesia:delete_table(ibo_boxdata),
    {atomic, ok} = mnesia:delete_table(ibo_boxindex),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok = mnesia:delete_schema(Nodes).

init_per_testcase(Case, Config) when Case =:= send_to_deadbox_test ; Case =:= send_to_deadbox_strict_test ->
    start_router(?ROUTER_NAME),
    start_deadletter_processes(Config);
init_per_testcase(_, Config) ->
    box_server:start_link(#{name =>?BOX_NAME}),
    start_router(?ROUTER_NAME),
    start_deadletter_processes(Config).

end_per_testcase(Case, _Config) when Case =:= send_to_box_deadrouter_restart_test ; Case =:= shutdown_restart_test->
    box_server:stop(?BOX_NAME),
    xbo_router:stop(?ROUTER_NAME),
    delete_boxstore();
end_per_testcase(_, Config) ->
    box_server:stop(?BOX_NAME),
    xbo_router:stop(?ROUTER_NAME),
    delete_deadletterstore(),
    stop_deadletter_processes(Config),
    delete_boxstore(),
    ok.

%%%%%===================================================================
%%%%% Deadletter Tests
%%%%%===================================================================
start_deadletter_test(_Config) ->
    ok.

send_to_box_deadrouter_test(_Config) ->
    XBO = ?NEWXBO2, %XBO is adressed to Router2, but Router 2 is not yet started
    {error, "no router available"} = xbo_router:strict_process_xbo(XBO, 1, ?BOX_NAME),
    0 = deadletter_server:get_size(),
    ok = xbo_router:process_xbo(XBO, 1, ?BOX_NAME),
    1 = deadletter_server:get_size(),
    check_box_empty(),

    start_router(?ROUTER2_NAME),
    ct_helper:waitms(1000), % tickrate of deadletter is 500ms, wait till tick and global got updated and the deadletter server started a worker to send the xbo on its way again
    check_box_notempty(),
    0 = deadletter_server:get_size(),
    ok.

send_to_box_deadrouter_restart_test(Config) -> % items in deadletter has to survive the restart of the deadletter server
    XBO = ?NEWXBO2, %XBO is adressed to Router2, but Router 2 is not yet started
    {error, "no router available"} = xbo_router:strict_process_xbo(XBO, 1, ?BOX_NAME),
    0 = deadletter_server:get_size(),
    ok = xbo_router:process_xbo(XBO, 1, ?BOX_NAME),
    1 = deadletter_server:get_size(),
    check_box_empty(),
    C1 = stop_deadletter_processes(Config),
    C2 = start_deadletter_processes(C1),
    start_router(?ROUTER2_NAME),
    ct_helper:waitms(1000), % tickrate of deadletter is 500ms, wait till tick and global got updated and the deadletter server started a worker to send the xbo on its way again
    check_box_notempty(),
    0 = deadletter_server:get_size(),
    stop_deadletter_processes(C2),
    ok.

send_to_deadbox_test(_Config) ->
    XBO = ?NEWXBO,
    {error, "cannot reach destination"} = xbo_router:super_strict_process_xbo(XBO, 1, ?BOX_NAME),
    0 = deadletter_server:get_size(),
    ok = xbo_router:process_xbo(XBO, 1, ?BOX_NAME),
    1 = deadletter_server:get_size(),
    check_box_empty(),

    box_server:start_link(#{name =>?BOX_NAME}),
    ct_helper:waitms(1000),
    check_box_notempty(),
    0 = deadletter_server:get_size(),
    ok.

send_to_deadbox_strict_test(_Config) ->
    XBO = ?NEWXBO,
    {error, "cannot reach destination"} = xbo_router:super_strict_process_xbo(XBO, 1, ?BOX_NAME),
    0 = deadletter_server:get_size(),
    ok = xbo_router:strict_process_xbo(XBO, 1, ?BOX_NAME),  % normal and strict should work, as only superstrict waits till the destination is reached as well
    1 = deadletter_server:get_size(),
    check_box_empty(),

    box_server:start_link(#{name =>?BOX_NAME}),
    ct_helper:waitms(1000),
    check_box_notempty(),
    0 = deadletter_server:get_size(),
    ok.

shutdown_restart_test(Config) -> % deadletter server should send its content to the iactors after restarting
    XBO = ?NEWXBO2, %XBO is adressed to Router2, but Router 2 is not yet started
    0 = deadletter_server:get_size(),
    ok = xbo_router:process_xbo(XBO, 1, ?BOX_NAME),
    1 = deadletter_server:get_size(),

    C2 = stop_deadletter_processes(Config),
    ct_helper:wait(),
    C3 = start_deadletter_processes(C2),
    start_router(?ROUTER2_NAME),
    ct_helper:waitms(1000), % tickrate of deadletter is 500ms, wait till tick and global got updated and the deadletter server started a worker to send the xbo on its way again
    check_box_notempty(),
    0 = deadletter_server:get_size(),
    _C4 = stop_deadletter_processes(C3),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
start_deadletter_processes(Config) ->
    {'ok', Pid} = deadletter_sup:start_link(#{tickrate=>500}),
    [{sup,Pid} | Config].

start_router(Name) ->
    xbo_router:start_link(#{name => Name, allowed => [?BOX_NAME,<<"blub_server">>,<<"another_server">>]}).

stop_deadletter_processes(Config) ->
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

check_box_empty() ->
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok.

check_box_notempty() ->
    true = 0 < ct_helper:get_recordcount_in_table(ibo_boxdata),
    true = 0 < ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok.

delete_deadletterstore() ->
    case deadletter_server:get_size() of
        0 -> ok;
        Size when Size > 0 ->
            {ok, _} = dets:open_file(deadletter_store),
            ok = dets:delete_all_objects(deadletter_store),
            ok = dets:close(deadletter_store)
    end.

delete_boxstore() ->
    mnesia:clear_table(ibo_boxdata),
    mnesia:clear_table(ibo_boxindex),
    ok.