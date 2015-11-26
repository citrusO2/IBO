%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2015 12:49
%%%-------------------------------------------------------------------
-module(xbo_router_SUITE).
-author("Florian").

-include("xbo_ct_macros.hrl").

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).

%% API
-export([sent_to_test/1, sent_to_baddestination_test/1, sent_to_wrongdomain_test/1]).

all() -> [sent_to_test, sent_to_baddestination_test, sent_to_wrongdomain_test].

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
sent_to_test(_Config) ->
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    XBO = ?XBO,
    XBOstepnr = 1,
    XBOstep = lists:nth(XBOstepnr, XBO#ibo_xbo.steps),
    BoxGroup = XBOstep#ibo_xbostep.local,
    StepDescription = XBOstep#ibo_xbostep.description,
    XBOid = XBO#ibo_xbo.id,
    XBOtemplate = XBO#ibo_xbo.template,

    % sent to box_server via router
    ok = xbo_router:process_xbo(XBO, 1, #ibo_xbostepdata{stepnr = 1}, "box_server"),

    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    Record1 = ct_helper:read_transactional(ibo_boxdata, XBOid),
    Record2 = ct_helper:read_transactional(ibo_boxindex, BoxGroup),
    ct_helper:print_var("Record1", Record1),
    ct_helper:print_var("Record2", Record2),

    {ibo_boxdata, XBOid, XBOstepnr, _ } = Record1,
    {ibo_boxindex, BoxGroup, [
        {ibo_boxindex_elementpreview, XBOid, XBOtemplate, StepDescription, _}
    ]} = Record2,
    ok.

sent_to_baddestination_test(_Config) ->
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    {error, "Destination is not allowed"} = xbo_router:process_xbo(?XBO, 1, #ibo_xbostepdata{stepnr = 1}, "black_hole"),
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok.

sent_to_wrongdomain_test(_Config) ->
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    {error,{check_xbo,"Step is for a different domain"}} = xbo_router:process_xbo(?FAILXBO_WRONGDOMAIN, 1, #ibo_xbostepdata{stepnr = 1}, "box_server"),
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok.