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
-include("../src/error/error_records.hrl"). % needed for error server to work, otherwise error handling of box-server is faulty (error server sends error -> box does not remove xbo)

%% API
-export([]).

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).

%% API
-export([cjump_test1/1, cjump_test2/1, cjump_xcondition1/1, cjump_xcondition2/1, xcondition_string_tests/1]).

all() -> [cjump_test1, cjump_test2, cjump_xcondition1, cjump_xcondition2, xcondition_string_tests].

init_per_suite(Config) ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    ct_helper:create_table_for_record(ibo_boxdata, record_info(fields, ibo_boxdata), Nodes),
    ct_helper:create_table_for_record(ibo_boxindex, record_info(fields, ibo_boxindex), Nodes),
    ct_helper:create_table_for_record(ibo_errordata, record_info(fields, ibo_errordata), Nodes),

    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:start(),
    mnesia:wait_for_tables([ibo_boxdata, ibo_boxindex], 5000),
    Config.

end_per_suite(_Config) ->
    Nodes = [node()],
    {atomic, ok} = mnesia:delete_table(ibo_boxdata),
    {atomic, ok} = mnesia:delete_table(ibo_boxindex),
    {atomic, ok} = mnesia:delete_table(ibo_errordata),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok = mnesia:delete_schema(Nodes).

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    box_server:start_link(#{name =>?BOX_NAME}),
    xbo_router:start_link(#{name => ?ROUTER_NAME, allowed => [?BOX_NAME, <<"another_server">>, <<"blub_server">>]}),
    error_server:start_link(?ERROR_SERVER_NAME),
    Config.

end_per_testcase(_, _Config) ->
    mnesia:clear_table(ibo_boxdata),
    mnesia:clear_table(ibo_boxindex),
    mnesia:clear_table(ibo_errordata),
    box_server:stop(?BOX_NAME),
    xbo_router:stop(?ROUTER_NAME),
    ok.

%%%===================================================================
%%% XBO Tests
%%%===================================================================
cjump_test1(_Config)->
    XBOstepnr = 1,
    XBO = ?LIBTEST1XBO,
    cjump_yes_path(XBO, XBOstepnr).

cjump_test2(_Config)->
    XBOstepnr = 1,
    XBO = ?LIBTEST1XBO,
    cjump_no_path(XBO, XBOstepnr).

cjump_xcondition1(_Config)->
    XBOstepnr = 1,
    XBO = ?LIBTEST3XBO,
    cjump_yes_path(XBO, XBOstepnr).

cjump_xcondition2(_Config)->
    XBOstepnr = 1,
    XBO = ?LIBTEST3XBO,
    cjump_no_path(XBO, XBOstepnr).

-define(REASON, #{<<"reason">> => <<"DefaultReason">>}).

xcondition_string_tests(_CONFIG)->
    Cases = [
        {[1, <<"yesno">>, <<"equal">>,<<"yes">>], ?REASON#{<<"yesno">> => <<"yes">>}, true},
        {[1, <<"yesno">>, <<"equal">>,<<"yes">>], ?REASON#{<<"yesno">> => <<"no">>}, false},
        {[1, <<"yesno">>, <<"unequal">>,<<"yes">>], ?REASON#{<<"yesno">> => <<"yes">>}, false},
        {[1, <<"yesno">>, <<"unequal">>,<<"yes">>], ?REASON#{<<"yesno">> => <<"no">>}, true},
        {[1, <<"yesno">>, <<"starts with">>,<<"y">>], ?REASON#{<<"yesno">> => <<"yes">>}, true},
        {[1, <<"yesno">>, <<"starts with">>,<<"y">>], ?REASON#{<<"yesno">> => <<"no">>}, false},
        {[1, <<"yesno">>, <<"starts with">>,<<"e">>], ?REASON#{<<"yesno">> => <<"yes">>}, false},
        {[1, <<"yesno">>, <<"starts with">>,<<"e">>], ?REASON#{<<"yesno">> => <<"no">>}, false},
        {[1, <<"yesno">>, <<"contains">>,<<"4138asfdaadfsd4">>], ?REASON#{<<"yesno">> => <<"no">>}, false},
        {[1, <<"yesno">>, <<"ends with">>,<<"s">>], ?REASON#{<<"yesno">> => <<"yes">>}, true},
        {[1, <<"yesno">>, <<"ends with">>,<<"s">>], ?REASON#{<<"yesno">> => <<"no">>}, false},
        {[1, <<"yesno">>, <<"ends with">>,<<"e">>], ?REASON#{<<"yesno">> => <<"yes">>}, false},
        {[1, <<"yesno">>, <<"ends with">>,<<"e">>], ?REASON#{<<"yesno">> => <<"no">>}, false},
        {[1, <<"yesno">>, <<"contains">>,<<"e">>], ?REASON#{<<"yesno">> => <<"yes">>}, true},
        {[1, <<"yesno">>, <<"contains">>,<<"e">>], ?REASON#{<<"yesno">> => <<"no">>}, false},
        {[1, <<"yesno">>, <<"contains">>,<<"y">>], ?REASON#{<<"yesno">> => <<"yes">>}, true},
        {[1, <<"yesno">>, <<"contains">>,<<"y">>], ?REASON#{<<"yesno">> => <<"no">>}, false},
        {[1, <<"yesno">>, <<"these are not the operators you are looking for">>,<<"y">>], ?REASON#{<<"yesno">> => <<"no">>}, error}
    ],
    lists:foreach(fun(Case) ->
        cjump_xcond(Case),
        mnesia:clear_table(ibo_boxdata),
        mnesia:clear_table(ibo_boxindex),
        mnesia:clear_table(ibo_errordata)
        end, Cases),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
cjump_yes_path(XBO, XBOstepnr) ->
    ok = box_server:process_xbo(?BOX_NAME, XBO, XBOstepnr),
    ct_helper:wait(),
    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    {ok, xbo_end} = box_server:execute_xbo(?BOX_NAME, XBO#ibo_xbo.id, #{<<"reason">> => <<"There is no alternative">>, <<"yesno">> => <<"yes">>}),

    ct_helper:wait(),
    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),    % data gets deleted after successfull execution
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex).   % index for group does not get deleted, only updated

cjump_no_path(XBO, XBOstepnr) ->
    ok = box_server:process_xbo(?BOX_NAME, XBO, XBOstepnr),
    ct_helper:wait(),
    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),

    {ok, xbo_send} = box_server:execute_xbo(?BOX_NAME, XBO#ibo_xbo.id, #{<<"reason">> => <<"There is an alternative">>, <<"yesno">> => <<"no">>}),
    ct_helper:wait(),
    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),    % data gets send to itself again
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex).

cjump_xcond({XCondition, Data, IsSuccessful} = Case) ->
    XBO = ?LIBTESTDYNXBO_XCONDITION(XCondition),
    XBOstepnr = 1,
    ct_helper:print_var("Case", Case),
    cjump_xcond(XBO, XBOstepnr, Data, IsSuccessful).

cjump_xcond(XBO, XBOstepnr, Data, IsSuccessful) ->
    ok = box_server:process_xbo(?BOX_NAME, XBO, XBOstepnr),
    ct_helper:wait(),
    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    0 = ct_helper:get_recordcount_in_table(ibo_errordata),
    {Status, Type} = box_server:execute_xbo(?BOX_NAME, XBO#ibo_xbo.id, Data),

    ct_helper:wait(),
    if
        IsSuccessful =:= error  ->
            Status = error,
            Type = xbo_error,
            0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
            1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
            1 = ct_helper:get_recordcount_in_table(ibo_errordata);
        IsSuccessful ->
            Status = ok,
            Type = xbo_end,
            0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
            1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
            0 = ct_helper:get_recordcount_in_table(ibo_errordata);
        not IsSuccessful ->
            Status = ok,
            Type = xbo_send,
            1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
            1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
            0 = ct_helper:get_recordcount_in_table(ibo_errordata)
    end,
    ok.
    %#{<<"reason">> => <<"There is an alternative">>, <<"yesno">> => <<"no">>}

