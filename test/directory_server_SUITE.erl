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
-define(USER,#ibo_user{username="ff",firstname="Fabian",lastname="Froelich"}).
-define(NEWUSER,#ibo_user{username="dd",firstname="Doris",lastname="Dührwald"}).


%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([get_user_test/1,
    get_user_fail_test/1,
    write_user_test/1,
    write_user_fail_test/1,
    search_user_test/1]).

all() -> [get_user_test,
    get_user_fail_test,
    write_user_test,
    write_user_fail_test,
    search_user_test].

init_per_suite(Config) ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(ibo_user,
        [{attributes, record_info(fields, ibo_user)},
            {disc_copies, Nodes},
            {type, set}]),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    mnesia:start(),
    mnesia:wait_for_tables([ibo_user],5000),
    Config.

end_per_suite(_Config) ->
    mnesia:delete_table(ibo_user),
    mnesia:stop().

init_per_testcase(_, Config) -> % first argument = name of the testcase as atom, Config = Property list
    directory_server:start_link(),
    add_user_to_mnesia(?USER),
    Config.

end_per_testcase(_, _Config) ->
    mnesia:clear_table(ibo_user),
    directory_server:stop().

%%%===================================================================
%%% Tests
%%%===================================================================
get_user_test(_Config) ->
    User1 = ?USER,
    User2 = directory_server:get_user(User1#ibo_user.username),
    io:format("User1: ~p~n",[User1]),
    io:format("User2: ~p~n",[User2]),
    true = User1 =:= User2,
    ok.

get_user_fail_test(_Config) ->
    Out = directory_server:get_user("MiauMiau"),
    io:format("Out: ~p~n",[Out]),
    true = Out =:= not_found,
    ok.

write_user_test(_Config) ->
    NewUser = ?NEWUSER,
    ok = directory_server:write_user(NewUser),
    DbUser = directory_server:get_user(NewUser#ibo_user.username),
    true = NewUser =:= DbUser,
    ok.

write_user_fail_test(_Config) ->
    Out = directory_server:write_user({"Crazy","NotUsed","Wrong Record"}),
    io:format("Out: ~p~n",[Out]),
    {error, _} = Out,
    ok.

search_user_test(_Config) ->
    User1 = ?USER,
    [User2] = directory_server:search_user("Froe"),
    [] = directory_server:search_user("Dühr"),
    io:format("User1: ~p~n",[User1]),
    io:format("User2: ~p~n",[User2]),
    true = User1 =:= User2,

    add_user_to_mnesia(?NEWUSER),
    [User3] = directory_server:search_user("Dühr"),
    true = User3 =:= ?NEWUSER,
    2 = length(directory_server:search_user("r")),

    remove_user_from_mnesia(User1),
    [User3] = directory_server:search_user("r"),
    ok.

%%%===================================================================
%%% Helper functions
%%%===================================================================
add_user_to_mnesia(User)->
    F = fun() ->
        mnesia:write(User)
        end,
    {atomic,ok} = mnesia:transaction(F),
    ok.

remove_user_from_mnesia(User)->
    F = fun() ->
        mnesia:delete({element(1,User),User#ibo_user.username})
        end,
    {atomic,ok} = mnesia:transaction(F),
    ok.