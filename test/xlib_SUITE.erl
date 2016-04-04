%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Dez 2015 18:59
%%%-------------------------------------------------------------------
-module(xlib_SUITE).
-author("Florian").

-include("xbo_ct_macros.hrl").

%% API
-export([]).

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).

%% API
-export([cjump_test1/1, cjump_test2/1]).

all() -> [cjump_test1, cjump_test2].

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

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    box_server:start_link(),
    xbo_router:start_link([["box_server","blub_server","another_server"]]),
    Config.

end_per_testcase(_, _Config) ->
    mnesia:clear_table(ibo_boxdata),
    mnesia:clear_table(ibo_boxindex),
    box_server:stop(),
    xbo_router:stop(),
    ok.

%%%===================================================================
%%% XBO Tests
%%%===================================================================
cjump_test1(_Config)->
    XBOstepnr = 1,
    XBO = ?LIBTEST1XBO,

    ok = box_server:process_xbo(XBO, XBOstepnr),
    ct_helper:wait(),
    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    {ok, xbo_end} = box_server:execute_xbo(XBO#ibo_xbo.id, #{<<"reason">> => <<"There is no alternative">>, <<"yesno">> => <<"yes">>}),

    ct_helper:wait(),
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),    % data gets deleted after successfull execution
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex).   % index for group does not get deleted, only updated

cjump_test2(_Config)->
    XBOstepnr = 1,
    XBO = ?LIBTEST1XBO,

    ok = box_server:process_xbo(XBO, XBOstepnr),
    ct_helper:wait(),
    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),

    {ok, xbo_send} = box_server:execute_xbo(XBO#ibo_xbo.id, #{<<"reason">> => <<"There is an alternative">>, <<"yesno">> => <<"no">>}),
    ct_helper:wait(),
    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex).
