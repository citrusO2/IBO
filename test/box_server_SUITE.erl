%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Nov 2015 13:51
%%%-------------------------------------------------------------------
-module(box_server_SUITE).
-author("Florian").

-include("../src/xbo/xbo_records.hrl").
-include("../src/web/box_records.hrl").

%% API
-export([]).

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([test/1]).
-include("test_helper_incl.erl").

all() -> [test].

init_per_suite(Config) ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    create_table_for_record(ibo_boxdata, record_info(fields, ibo_boxdata), Nodes),
    create_table_for_record(ibo_boxindex, record_info(fields, ibo_boxindex), Nodes),
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

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%===================================================================
%%% XBO Tests
%%%===================================================================
test(_Config) ->
    ok.
