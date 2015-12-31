%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dez 2015 19:29
%%%-------------------------------------------------------------------
-module(repo_server_SUITE).
-author("Florian").

-include("../src/box/box_records.hrl").
-include("../src/repo/repo_records.hrl").
-include("template_ct_macros.hrl").

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).

%% API
-export([store_template_test/1, start_template_test/1]).
all() -> [store_template_test, start_template_test].

init_per_suite(Config) ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    ct_helper:create_table_for_record(ibo_boxdata, record_info(fields, ibo_boxdata), Nodes),
    ct_helper:create_table_for_record(ibo_boxindex, record_info(fields, ibo_boxindex), Nodes),
    ct_helper:create_table_for_record(ibo_repo_template, record_info(fields, ibo_repo_template), Nodes),

    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:start(),
    mnesia:wait_for_tables([ibo_boxdata, ibo_boxindex, ibo_repo_template], 5000),
    Config.

end_per_suite(_Config) ->
    Nodes = [node()],
    {atomic, ok} = mnesia:delete_table(ibo_boxdata),
    {atomic, ok} = mnesia:delete_table(ibo_boxindex),
    {atomic, ok} = mnesia:delete_table(ibo_repo_template),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok = mnesia:delete_schema(Nodes).

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    box_server:start_link(),
    xbo_router:start_link([["box_server","blub_server","another_server"]]),
    repo_server:start_link({["xbo_router"], ["should_be_deadletter_server"],"repo1",1}),
    Config.

end_per_testcase(_, _Config) ->
    mnesia:clear_table(ibo_boxdata),
    mnesia:clear_table(ibo_boxindex),
    mnesia:clear_table(ibo_repo_template),
    box_server:stop(),
    xbo_router:stop(),
    repo_server:stop(),
    ok.

%%%===================================================================
%%% XBO Tests
%%%===================================================================
store_template_test(_Config) ->
    Template = ?TEMPLATE_TESTTEMPLATE1,

    0 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    ok = repo_server:store_template(Template),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    ok.

start_template_test(_Config) ->
    Template = ?TEMPLATE_TESTTEMPLATE1,
    User = ?MARKETINGUSER,

    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),

    ok = repo_server:store_template(Template),
    ok = repo_server:start_template(User, Template#ibo_repo_template.template),
    ct_helper:wait(1),

    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok.