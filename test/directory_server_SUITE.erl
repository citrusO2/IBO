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
-define(USER, #ibo_user{username = "ff", firstname = "Fabian", lastname = "Froelich"}).
-define(NEWUSER, #ibo_user{username = "dd", firstname = "Doris", lastname = "Dührwald"}).
-define(GROUP, #ibo_group{groupname = "ACME_Corporation", groupdescription = "This should be the root group"}).
-define(NEWGROUP, #ibo_group{groupname = "Marketing", groupdescription = "Group for the Unit Marketing", parent = "ACME_Corporation"}).

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([get_user_test/1, get_user_fail_test/1,
    write_user_test/1, write_user_fail_test/1,
    search_user_test/1,
    get_group_test/1, get_group_fail_test/1,
    write_group_test/1, write_group_fail_test/1,
    search_group_test/1]).

all() -> [get_user_test, get_user_fail_test,
    write_user_test, write_user_fail_test,
    search_user_test,
    get_group_test, get_group_fail_test,
    write_group_test, write_group_fail_test,
    search_group_test].

init_per_suite(Config) ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(ibo_user,
        [{attributes, record_info(fields, ibo_user)},
            {disc_copies, Nodes},
            {type, set}]),
    mnesia:create_table(ibo_group,
        [{attributes, record_info(fields, ibo_group)},
            {disc_copies, Nodes},
            {type, set}]),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:start(),
    mnesia:wait_for_tables([ibo_user], 5000),
    Config.

end_per_suite(_Config) ->
    mnesia:delete_table(ibo_user),
    mnesia:delete_table(ibo_group),
    mnesia:stop().

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    directory_server:start_link(),
    add_record_to_mnesia(?USER),
    add_record_to_mnesia(?GROUP),
    Config.

end_per_testcase(_, _Config) ->
    mnesia:clear_table(ibo_user),
    mnesia:clear_table(ibo_group),
    directory_server:stop().

%%%===================================================================
%%% User Tests
%%%===================================================================
get_user_test(_Config) ->
    Record1 = ?USER,
    Record2 = directory_server:get_user(Record1#ibo_user.username),
    print_var("Record1", Record1),
    print_var("Record2", Record2),
    true = Record1 =:= Record2,
    ok.

get_user_fail_test(_Config) ->
    Out = directory_server:get_user("MiauMiau"),
    print_var("Out", Out),
    true = Out =:= not_found,
    ok.

write_user_test(_Config) ->
    Record1 = ?NEWUSER,
    ok = directory_server:write_user(Record1),
    Record2 = directory_server:get_user(Record1#ibo_user.username),
    true = Record1 =:= Record2,
    ok.

write_user_fail_test(_Config) ->
    Out = directory_server:write_user({"Crazy", "NotUsed", "Wrong Record"}),
    print_var("Out", Out),
    {error, _} = Out,
    ok.

search_user_test(_Config) ->
    Record1 = ?USER,
    [Record2] = directory_server:search_user("Froe"),
    [] = directory_server:search_user("Dühr"),
    print_var("Record1", Record1),
    print_var("Record2", Record2),
    true = Record1 =:= Record2,

    add_record_to_mnesia(?NEWUSER),
    [Record3] = directory_server:search_user("Dühr"),
    true = Record3 =:= ?NEWUSER,
    2 = length(directory_server:search_user("")),

    remove_record_from_mnesia(Record1),
    [Record3] = directory_server:search_user("r"),
    ok.

%%%===================================================================
%%% Group Tests
%%%===================================================================
get_group_test(_Config) ->
    Record1 = ?GROUP,
    Record2 = directory_server:get_group(Record1#ibo_group.groupname),
    print_var("Record1", Record1),
    print_var("Record2", Record2),
    true = Record1 =:= Record2,
    ok.

get_group_fail_test(_Config) ->
    Out = directory_server:get_group("MiauMiau"),
    print_var("Out", Out),
    true = Out =:= not_found,
    ok.

write_group_test(_Config) ->
    Record1 = ?NEWGROUP,
    ok = directory_server:write_group(Record1),
    Record2 = directory_server:get_group(Record1#ibo_group.groupname),
    true = Record1 =:= Record2,
    ok.

write_group_fail_test(_Config) ->
    Out = directory_server:write_group({"Crazy", "NotUsed", "Wrong Record"}),
    print_var("Out", Out),
    {error, _} = Out,
    ok.

search_group_test(_Config) ->
    Record1 = ?GROUP,
    [Record2] = directory_server:search_group("ACME"),
    [] = directory_server:search_group("Mark"),
    print_var("Record1", Record1),
    print_var("Record2", Record2),
    true = Record1 =:= Record2,

    add_record_to_mnesia(?NEWGROUP),
    [Record3] = directory_server:search_group("Mark"),
    true = Record3 =:= ?NEWGROUP,
    2 = length(directory_server:search_group("")),

    remove_record_from_mnesia(Record1),
    [Record3] = directory_server:search_group("r"),
    ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================
add_record_to_mnesia(Record) ->
    F = fun() ->
        mnesia:write(Record)
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

remove_record_from_mnesia(Record) ->
    F = fun() ->
        mnesia:delete({element(1, Record), element(2, Record)})
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

print_var(Var, VarName) ->
    io:format("~p: ~p~n", [VarName, Var]),
    ok.