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
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1, version_counter_test/1]).

%% API
-export([store_template_test/1, start_template_test/1, retrieve_template_test/1, delete_template_test/1]).
all() -> [store_template_test, start_template_test, retrieve_template_test, version_counter_test, delete_template_test].

init_per_suite(Config) ->
    ct_helper:init_mnesia(),
    Config.

end_per_suite(_Config) ->
    ct_helper:deinit_mnesia().

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    box_server:start_link(?BOX_NAME),
    xbo_router:start_link(?ROUTER_ARGS),
    repo_server:start_link(?REPO_ARGS),
    Config.

end_per_testcase(_, _Config) ->
    mnesia:clear_table(ibo_boxdata),
    mnesia:clear_table(ibo_boxindex),
    mnesia:clear_table(ibo_repo_template),
    mnesia:clear_table(ibo_repo_template_old),
    box_server:stop(?BOX_NAME),
    xbo_router:stop(?ROUTER_NAME),
    repo_server:stop(?REPO_NAME),
    ok.

%%%===================================================================
%%% XBO Tests
%%%===================================================================
store_template_test(_Config) ->
    Template = ?TEMPLATE_TESTTEMPLATE1,

    0 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    ok = repo_server:store_template(?REPO_NAME, Template, ?REPO_MANAGEGROUPS),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    ok.

start_template_test(_Config) ->
    Template = ?TEMPLATE_TESTTEMPLATE1,
    User = ?MARKETINGUSER,

    0 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    0 = ct_helper:get_recordcount_in_table(ibo_boxindex),

    ok = repo_server:store_template(?REPO_NAME, Template, ?REPO_MANAGEGROUPS),
    ok = repo_server:start_template(?REPO_NAME, [<<"marketing">>], User#ibo_user.username, Template#ibo_repo_template.name),
    ct_helper:wait(),

    1 = ct_helper:get_recordcount_in_table(ibo_boxdata),
    1 = ct_helper:get_recordcount_in_table(ibo_boxindex),
    ok.

retrieve_template_test(_Config) ->
    0 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    Template1 = ?TEMPLATE_TESTTEMPLATE1,
    Template2 = ?TEMPLATE_TESTTEMPLATE2,
    Template3 = ?TEMPLATE_TESTTEMPLATE3,
    ok = repo_server:store_template(?REPO_NAME, Template1, ?REPO_MANAGEGROUPS),
    ok = repo_server:store_template(?REPO_NAME, Template2, ?REPO_MANAGEGROUPS),
    ok = repo_server:store_template(?REPO_NAME, Template3, ?REPO_MANAGEGROUPS),
    3 = ct_helper:get_recordcount_in_table(ibo_repo_template),

    User = ?MARKETINGUSER,
    Response = repo_server:get_templatelist(?REPO_NAME, User#ibo_user.groups),  % should use directory_server's resolve group! -> groups is only used here to avoid using a box_server in this test
    2 = length(Response),
    true = lists:member({<<"marketingbudgetdecision">>,<<"marketingbudgetdecisiondescription">>}, Response),
    true = lists:member({<<"malala">>, <<"malaladescription">>}, Response),
    false = lists:member({<<"wuhaha">>, <<"wuhahadescription">>}, Response),
    ok.

version_counter_test(_Config) ->    % overwriting an existing template should give the new template a higher version number
    0 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    0 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),
    Template = ?TEMPLATE_TESTTEMPLATE4,

    ok = repo_server:store_template(?REPO_NAME, Template, ?REPO_MANAGEGROUPS),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    0 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),
    StoredTemplate1 = db:read_transactional(ibo_repo_template, Template#ibo_repo_template.name),
    1 = StoredTemplate1#ibo_repo_template.version,

    ok = repo_server:store_template(?REPO_NAME, Template, ?REPO_MANAGEGROUPS),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),
    StoredTemplate2 = db:read_transactional(ibo_repo_template, Template#ibo_repo_template.name),
    2 = StoredTemplate2#ibo_repo_template.version,

    ok = repo_server:store_template(?REPO_NAME, Template, ?REPO_MANAGEGROUPS),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    2 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),
    StoredTemplate3 = db:read_transactional(ibo_repo_template, Template#ibo_repo_template.name),
    3 = StoredTemplate3#ibo_repo_template.version,
    ok.

delete_template_test(_Config)->
    0 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    0 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),
    Template1 = ?TEMPLATE_TESTTEMPLATE1,
    Template2 = ?TEMPLATE_TESTTEMPLATE2,
    Template3 = ?TEMPLATE_TESTTEMPLATE3,
    ok = repo_server:store_template(?REPO_NAME, Template1, ?REPO_MANAGEGROUPS),
    ok = repo_server:store_template(?REPO_NAME, Template2, ?REPO_MANAGEGROUPS),
    ok = repo_server:store_template(?REPO_NAME, Template3, ?REPO_MANAGEGROUPS),
    3 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    0 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),

    ok = repo_server:delete_template(?REPO_NAME, Template1#ibo_repo_template.name, ?REPO_MANAGEGROUPS),
    2 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),

    not_found = repo_server:delete_template(?REPO_NAME, <<"asdkjfalsdjfaödsf">>, ?REPO_MANAGEGROUPS),
    2 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),

    {error, _} = repo_server:delete_template(?REPO_NAME, Template2#ibo_repo_template.name, [<<"asdfadsfasdf">>]),
    2 = ct_helper:get_recordcount_in_table(ibo_repo_template),
    1 = ct_helper:get_recordcount_in_table(ibo_repo_template_old),
    ok.