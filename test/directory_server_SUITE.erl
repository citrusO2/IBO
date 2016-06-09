%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2015 19:34
%%%-------------------------------------------------------------------
-module(directory_server_SUITE).
-author("Florian").

-include("../src/directory/directory_records.hrl").
-define(USER, #ibo_user{username = <<"ff">>, firstname = <<"Fabian">>, lastname = <<"Froelich">>}).
-define(NEWUSER1, #ibo_user{username = <<"dd">>, firstname = <<"Doris">>, lastname = <<"Dührwald">>, groups = [<<"Marketing">>]}).
-define(NEWUSER2, #ibo_user{username = <<"mm">>, firstname = <<"Maris">>, lastname = <<"Mohrwald">>, groups = [<<"IT_Interns">>, <<"Marketing">>]}).
-define(NEWUSER3, #ibo_user{username = <<"xx">>, firstname = <<"Xavier">>, lastname = <<"Xund">>, groups = [<<"IT_Admins">>]}).
-define(USERLIST, [?USER, ?NEWUSER1, ?NEWUSER2, ?NEWUSER3]).

-define(GROUP, #ibo_group{name = <<"ACME_Corporation">>, description = <<"This should be the root group">>}).
-define(NEWGROUP1, #ibo_group{name = <<"Marketing">>, description = <<"Group for the Unit Marketing">>, parents = [<<"ACME_Corporation">>]}).
-define(NEWGROUP2, #ibo_group{name = <<"IT">>, description = <<"Group for the IT">>, parents = [<<"ACME_Corporation">>]}).
-define(NEWGROUP3, #ibo_group{name = <<"IT_Interns">>, description = <<"Group for the IT interns">>, parents = [<<"IT">>]}).
-define(NEWGROUP4, #ibo_group{name = <<"IT_Admins">>, description = <<"Group for the IT admins">>, parents = [<<"IT">>]}).
-define(GROUPLIST, [?GROUP, ?NEWGROUP1, ?NEWGROUP2, ?NEWGROUP3, ?NEWGROUP4]).

-define(CIRCULARGROUP1, #ibo_group{name = <<"IT_small_circle">>, description = <<"Circular Self Reference Group for IT">>, parents = [<<"IT_small_circle">>, <<"IT">>]}).
-define(CIRCULARGROUP2, #ibo_group{name = <<"IT_big_circle">>, description = <<"Circular Self Reference Group via other Group">>, parents = [<<"IT_big_circle_helper">>, <<"IT">>]}).
-define(CIRCULARGROUP3, #ibo_group{name = <<"IT_big_circle_helper">>, description = <<"Circular Self Reference Group via other Group">>, parents = [<<"IT_big_circle">>]}).
-define(CIRCULARUSER1, #ibo_user{username = <<"hh">>, firstname = <<"Heinrich">>, lastname = <<"Harrer">>, groups = [<<"IT_small_circle">>]}).
-define(CIRCULARUSER2, #ibo_user{username = <<"ll">>, firstname = <<"Ludwig">>, lastname = <<"Luminga">>, groups = [<<"IT_big_circle">>]}).
-define(CIRCULARLIST, [?CIRCULARGROUP1, ?CIRCULARGROUP2, ?CIRCULARGROUP3, ?CIRCULARUSER1, ?CIRCULARUSER2]).

-define(REDUNDANDGROUPMEMBERSHIPUSER, #ibo_user{username = <<"oo">>, firstname = <<"Ori">>, lastname = <<"Osram">>, groups = [<<"IT_Admins">>,<<"IT">>]}).
-define(NOTEXISTENDGROUPUSER, #ibo_user{username = <<"qq">>, firstname = <<"Query">>, lastname = <<"Qualtinger">>, groups = [<<"LazyA__F__KS">>]}).

-define(DIRECTORY_NAME, <<"DIRECTORY1">>).
-define(DIRECTORY_ARGS, #{name => ?DIRECTORY_NAME}).

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([get_user_test/1, get_user_fail_test/1,
    search_user_test/1,
    get_group_test/1, get_group_fail_test/1,
    write_group_test/1, write_group_fail_test/1,
    search_group_test/1,
    create_user_test/1, get_user_info/1,
    resolve_usergroups_emptygroups_test/1,
    resolve_usergroups_correctresolve_test/1,
    resolve_circular_groups_test/1,
    redundant_groupmembership_test/1,
    unresolvable_group_test/1]).

all() -> [get_user_test, get_user_fail_test,
    search_user_test,
    get_group_test, get_group_fail_test,
    write_group_test, write_group_fail_test,
    search_group_test,
    create_user_test, get_user_info,
    resolve_usergroups_emptygroups_test,
    resolve_usergroups_correctresolve_test,
    resolve_circular_groups_test,
    redundant_groupmembership_test,
    unresolvable_group_test].

init_per_suite(Config) ->
    application:start(crypto),
    ct_helper:init_mnesia(),
    Config.

end_per_suite(_Config) ->
    application:stop(crypto),
    ct_helper:deinit_mnesia().

init_per_testcase(unresolvable_group_test, Config) ->
    directory_server:start_link(?DIRECTORY_ARGS),
    ct_helper:add_records_to_mnesia(?GROUPLIST),
    ct_helper:add_record_to_mnesia(?NOTEXISTENDGROUPUSER),
    Config;
init_per_testcase(redundant_groupmembership_test, Config) ->
    directory_server:start_link(?DIRECTORY_ARGS),
    ct_helper:add_records_to_mnesia(?GROUPLIST),
    ct_helper:add_record_to_mnesia(?REDUNDANDGROUPMEMBERSHIPUSER),
    Config;
init_per_testcase(resolve_circular_groups_test, Config) ->
    directory_server:start_link(?DIRECTORY_ARGS),
    ct_helper:add_records_to_mnesia(?GROUPLIST),
    ct_helper:add_records_to_mnesia(?CIRCULARLIST),
    Config;
init_per_testcase(Testcase, Config) when Testcase =:= resolve_usergroups_emptygroups_test; Testcase =:= resolve_usergroups_correctresolve_test ->
    directory_server:start_link(?DIRECTORY_ARGS),
    ct_helper:add_records_to_mnesia(?USERLIST),
    ct_helper:add_records_to_mnesia(?GROUPLIST),
    Config;
init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    directory_server:start_link(?DIRECTORY_ARGS),
    ct_helper:add_record_to_mnesia(?USER),
    ct_helper:add_record_to_mnesia(?GROUP),
    Config.

end_per_testcase(_, _Config) ->
    mnesia:clear_table(ibo_user),
    mnesia:clear_table(ibo_group),
    directory_server:stop(?DIRECTORY_NAME).

%%%===================================================================
%%% User Tests
%%%===================================================================
get_user_test(_Config) ->
    Record1 = ?USER,
    Record2 = directory_server:get_user(?DIRECTORY_NAME, Record1#ibo_user.username),
    ct_helper:print_var("Record1", Record1),
    ct_helper:print_var("Record2", Record2),
    true = Record1 =:= Record2,
    ok.

get_user_fail_test(_Config) ->
    Out = directory_server:get_user(?DIRECTORY_NAME, <<"MiauMiau">>),
    ct_helper:print_var("Out", Out),
    true = Out =:= not_found,
    ok.

search_user_test(_Config) ->
    Record1 = ?USER,
    [Record2] = directory_server:search_user(?DIRECTORY_NAME, <<"Froe">>),
    [] = directory_server:search_user(?DIRECTORY_NAME, <<"Dühr">>),
    ct_helper:print_var("Record1", Record1),
    ct_helper:print_var("Record2", Record2),
    true = Record1 =:= Record2,

    ct_helper:add_record_to_mnesia(?NEWUSER1),
    [Record3] = directory_server:search_user(?DIRECTORY_NAME, <<"Dühr">>),
    true = Record3 =:= ?NEWUSER1,
    2 = length(directory_server:search_user(?DIRECTORY_NAME, "")),
    2 = length(directory_server:search_user(?DIRECTORY_NAME, <<"">>)),

    ct_helper:remove_record_from_mnesia(Record1),
    [Record3] = directory_server:search_user(?DIRECTORY_NAME, <<"r">>),
    ok.

%%%===================================================================
%%% Group Tests
%%%===================================================================
get_group_test(_Config) ->
    Record1 = ?GROUP,
    Record2 = directory_server:get_group(?DIRECTORY_NAME, Record1#ibo_group.name),
    ct_helper:print_var("Record1", Record1),
    ct_helper:print_var("Record2", Record2),
    true = Record1 =:= Record2,
    ok.

get_group_fail_test(_Config) ->
    Out = directory_server:get_group(?DIRECTORY_NAME, <<"MiauMiau">>),
    ct_helper:print_var("Out", Out),
    true = Out =:= not_found,
    ok.

write_group_test(_Config) ->
    Record1 = ?NEWGROUP1,
    ok = directory_server:write_group(?DIRECTORY_NAME, Record1),
    Record2 = directory_server:get_group(?DIRECTORY_NAME, Record1#ibo_group.name),
    true = Record1 =:= Record2,
    ok.

write_group_fail_test(_Config) ->
    Out = directory_server:write_group(?DIRECTORY_NAME, {<<"Crazy">>, <<"NotUsed">>, <<"Wrong Record">>}),
    ct_helper:print_var("Out", Out),
    {error, _} = Out,
    ok.

search_group_test(_Config) ->
    Record1 = ?GROUP,
    [Record2] = directory_server:search_group(?DIRECTORY_NAME, <<"ACME">>),
    [] = directory_server:search_group(?DIRECTORY_NAME, <<"Mark">>),
    ct_helper:print_var("Record1", Record1),
    ct_helper:print_var("Record2", Record2),
    true = Record1 =:= Record2,

    ct_helper:add_record_to_mnesia(?NEWGROUP1),
    [Record3] = directory_server:search_group(?DIRECTORY_NAME, <<"Mark">>),
    true = Record3 =:= ?NEWGROUP1,
    2 = length(directory_server:search_group(?DIRECTORY_NAME, <<"">>)),

    ct_helper:remove_record_from_mnesia(Record1),
    [Record3] = directory_server:search_group(?DIRECTORY_NAME, <<"r">>),
    ok.

%%%===================================================================
%%% Higher User Tests
%%%===================================================================
create_user_test(_Config) ->
    Password = <<"MySecretPassword">>,
    Record1 = ?NEWUSER1,
    ct_helper:print_var("Record1", Record1),
    ok = directory_server:create_user(?DIRECTORY_NAME, Record1, Password),

    {ibo_user,<<"dd">>,<<"Doris">>,_,_,_} = Record2 = directory_server:get_user(?DIRECTORY_NAME, Record1#ibo_user.username),
    ct_helper:print_var("Record2", Record2),

    UpdatedUser = Record2#ibo_user{firstname = <<"Daniela">>},
    ct_helper:print_var("UpdatedUser", UpdatedUser),
    directory_server:update_user(?DIRECTORY_NAME, UpdatedUser, Password),

    {ibo_user,<<"dd">>,<<"Daniela">>,_,_,_} = Record3 = directory_server:get_user(?DIRECTORY_NAME, Record1#ibo_user.username),
    ct_helper:print_var("Record3", Record3),

    UpdatedUser2 = Record3#ibo_user{firstname = <<"Darsir">>},
    {error,"Incorrect password"} = directory_server:update_user(?DIRECTORY_NAME, UpdatedUser2, <<"Miau">>),
    ok.

get_user_info(_Config) ->
    Password = <<"MySecretPassword">>,
    Record1 = ?NEWUSER1,
    ok = directory_server:create_user(?DIRECTORY_NAME, Record1, Password),
    Response1 = directory_server:get_user_info(?DIRECTORY_NAME, Record1#ibo_user.username, Password),
    ct_helper:print_var("Response1",Response1),
    Response1 = Record1,
    {error, "Wrong password"} = directory_server:get_user_info(?DIRECTORY_NAME, Record1#ibo_user.username, <<"Plubla">>).

%%%===================================================================
%%% Recursive resolve groups tests (resolve_usergroups uses tail-recursive loop, wrapped in a mnesia transaction)
%%%===================================================================
resolve_usergroups_emptygroups_test(_Config) ->  % test that checks if the resolved usergroups equal to [] when the user is not member of any group
    [] = tripleresolve_groups(?USER),
    ok.

resolve_usergroups_correctresolve_test(_Config) -> % test if the users are resolved right
    check_resolved_groups(tripleresolve_groups(?NEWUSER1), [<<"Marketing">>,<<"ACME_Corporation">>]),
    check_resolved_groups(tripleresolve_groups(?NEWUSER2), [<<"Marketing">>,<<"ACME_Corporation">>,<<"IT_Interns">>, <<"IT">>]),
    check_resolved_groups(tripleresolve_groups(?NEWUSER3), [<<"IT_Admins">>, <<"IT">>,<<"ACME_Corporation">>]).

resolve_circular_groups_test(_Config) -> % test if circular dependencies are resolved without duplicates and without endless-loop
    check_resolved_groups(tripleresolve_groups(?CIRCULARUSER1), [<<"IT_small_circle">>, <<"IT">>,<<"ACME_Corporation">>]),
    check_resolved_groups(tripleresolve_groups(?CIRCULARUSER2), [<<"IT_big_circle">>,<<"IT_big_circle_helper">>, <<"IT">>,<<"ACME_Corporation">>]).

redundant_groupmembership_test(_Config) ->  % test if redundant groupmemberships are resolved only once
    check_resolved_groups(tripleresolve_groups(?REDUNDANDGROUPMEMBERSHIPUSER), [<<"IT_Admins">>, <<"IT">>,<<"ACME_Corporation">>]).

unresolvable_group_test(_Config) -> % test to see if server returns only given group when the group cannot be resolved
    check_resolved_groups(tripleresolve_groups(?NOTEXISTENDGROUPUSER), [<<"LazyA__F__KS">>]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
tripleresolve_groups(User) when is_record(User, ibo_user) ->
    Groups = directory_server:resolve_usergroups(?DIRECTORY_NAME, User),
    Groups = directory_server:resolve_usergroups(?DIRECTORY_NAME, User#ibo_user.username),
    Groups = directory_server:resolve_usergroups(?DIRECTORY_NAME, User#ibo_user.groups).

check_resolved_groups(ResolvedGroups, TargetGroups)-> % the resolved groups must be the same as the target groups -> except the order
    ct_helper:print_var("ResolvedGroups", ResolvedGroups),
    ct_helper:print_var("TargetGroups", TargetGroups),
    GroupSize = erlang:length(ResolvedGroups),
    GroupSize = erlang:length(TargetGroups),
    RGset = ordsets:from_list(ResolvedGroups),
    TGset = ordsets:from_list(TargetGroups),
    GroupSize = ordsets:size(RGset),
    GroupSize = ordsets:size(TGset),
    true = ordsets:is_subset(RGset, TGset),
    true = ordsets:is_subset(TGset, RGset),
    ok.