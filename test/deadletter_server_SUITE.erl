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
%-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([all/0]).

%% API
%%-export([store_case_test/1, store_routercase_test/1]).
%%all() -> [store_case_test, store_routercase_test].
all() -> [].

%%init_per_suite(Config) ->
%%    Nodes = [node()],
%%    ok = mnesia:create_schema(Nodes),
%%    rpc:multicall(Nodes, application, start, [mnesia]),
%%    ct_helper:create_table_for_record(ibo_deadletterdata, record_info(fields, ibo_deadletterdata), Nodes),
%%    rpc:multicall(Nodes, application, stop, [mnesia]),
%%    mnesia:start(),
%%    mnesia:wait_for_tables([ibo_deadletterdata], 5000),
%%    Config.
%%
%%end_per_suite(_Config) ->
%%    Nodes = [node()],
%%    {atomic, ok} = mnesia:delete_table(ibo_deadletterdata),
%%    rpc:multicall(Nodes, application, stop, [mnesia]),
%%    ok = mnesia:delete_schema(Nodes).
%%
%%init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
%%    xbo_router:start_link([["box_server","blub_server","another_server"]]),
%%    deadletter_server:start_link(),
%%    Config.
%%
%%end_per_testcase(_, _Config) ->
%%    mnesia:clear_table(ibo_deadletterdata),
%%    deadletter_server:stop(),
%%    xbo_router:stop(),
%%    ok.
%%
%%%%%===================================================================
%%%%% Deadletter Tests
%%%%%===================================================================
%%store_case_test(_Config) ->
%%    XBO = ?NEWXBO,
%%
%%    0 = ct_helper:get_recordcount_in_table(ibo_deadletterdata),
%%    ok = deadletter_server:process_xbo(XBO, 1, {error, "My Errorreason, because of test reasons"}),
%%    1 = ct_helper:get_recordcount_in_table(ibo_deadletterdata),
%%    ok.
%%
%%store_routercase_test(_Config) ->
%%    XBO = ?NEWXBO,
%%    XlibState = #xlib_state{xbo = XBO, current_stepdata = #ibo_xbostepdata{stepnr = 1}},
%%    0 = ct_helper:get_recordcount_in_table(ibo_deadletterdata),
%%
%%    ok = xbo_router:debug_xbo(XlibState, "Just a testreason error message"),
%%    ct_helper:wait(),
%%    1 = ct_helper:get_recordcount_in_table(ibo_deadletterdata),
%%
%%    ok.