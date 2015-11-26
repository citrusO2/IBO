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

-include("box_server_ct_macros.hrl").

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([process_xbo_emptybox_test/1, process_xbo_nonemptybox_test/1, process_xbo_duplication_test/1,process_xbo_check_test/1]).

all() -> [process_xbo_emptybox_test, process_xbo_nonemptybox_test, process_xbo_duplication_test, process_xbo_check_test].

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
    Config.

end_per_testcase(_, _Config) ->
    mnesia:clear_table(ibo_boxdata),
    mnesia:clear_table(ibo_boxindex),
    box_server:stop(),
    ok.

%%%===================================================================
%%% XBO Tests
%%%===================================================================
process_xbo_emptybox_test(_Config) ->
    XBO = ?XBO,
    XBOstepnr = 1,
    XBOstep = lists:nth(XBOstepnr, XBO#ibo_xbo.steps),
    BoxGroup = XBOstep#ibo_xbostep.local,
    StepDescription = XBOstep#ibo_xbostep.description,
    XBOid = XBO#ibo_xbo.id,
    XBOtemplate = XBO#ibo_xbo.template,

    ok = box_server:process_xbo(XBO, XBOstepnr),

    Record1 = ct_helper:read_transactional(ibo_boxdata, XBOid),
    Record2 = ct_helper:read_transactional(ibo_boxindex, BoxGroup),

    {ibo_boxdata, XBOid, XBO, XBOstepnr} = Record1,
    {ibo_boxindex, BoxGroup, [
        {ibo_boxindex_elementpreview, XBOid, XBOtemplate, StepDescription, _}
    ]} = Record2,
    ok.

process_xbo_nonemptybox_test(_Config) ->
    XBO = ?NEWXBO,
    XBOstepnr = 1,
    XBOstep = lists:nth(XBOstepnr, XBO#ibo_xbo.steps),
    BoxGroup = XBOstep#ibo_xbostep.local,
    StepDescription = XBOstep#ibo_xbostep.description,
    XBOid = XBO#ibo_xbo.id,
    XBOtemplate = XBO#ibo_xbo.template,

    ok = box_server:process_xbo(?XBO, XBOstepnr),
    ok = box_server:process_xbo(XBO, XBOstepnr),

    Record1 = ct_helper:read_transactional(ibo_boxdata, XBOid),
    Record2 = ct_helper:read_transactional(ibo_boxindex, BoxGroup),

    ct_helper:print_var("Record2", Record2),

    {ibo_boxdata, XBOid, XBO, XBOstepnr} = Record1,
    {ibo_boxindex, BoxGroup, [
        {ibo_boxindex_elementpreview, XBOid, XBOtemplate, StepDescription, _},
        {_, _, _, _, _}
    ]} = Record2,
    ok.

process_xbo_duplication_test(_Config) ->
    XBOstepnr = 1,
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok = Response1 = box_server:process_xbo(?XBO, XBOstepnr),
    ok = Response2 = box_server:process_xbo(?NEWXBO, XBOstepnr),
    2 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    {error, _} = Response3 = box_server:process_xbo(?XBO, XBOstepnr),
    {error, _} = Response4 = box_server:process_xbo(?NEWXBO, XBOstepnr),
    2 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ct_helper:print_var("Response1", Response1),
    ct_helper:print_var("Response2", Response2),
    ct_helper:print_var("Response3", Response3),
    ct_helper:print_var("Response4", Response4),
    ok.

process_xbo_check_test(_Config) ->
    XBOstepnr = 1,
    XBOfailstepnr = 2,
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),

    {error, _} = box_server:process_xbo(?XBO, XBOfailstepnr),
    {error, _} = box_server:process_xbo(?NEWXBO, XBOfailstepnr),
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),

    {error, _} = box_server:process_xbo(?FAILXBO, XBOstepnr),
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),

    {error, _} = box_server:process_xbo(?XBO#ibo_xbo{id = ""}, XBOstepnr),
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok.