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

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([get_user_test/1,test2/1]).

all()->
    [get_user_test,test2].

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
    User1 = ?USER,
    directory_server:start_link(),
    F = fun() ->
        mnesia:write(User1)
        end,
    {atomic,ok} = mnesia:transaction(F),
    [{row,User1}|Config].

end_per_testcase(_, Config) ->
    User1 = ?config(row,Config),
    F = fun() ->
        mnesia:delete({element(1,User1),User1#ibo_user.username})
        end,
    mnesia:transaction(F),
    directory_server:stop().

%%%===================================================================
%%% Tests
%%%===================================================================
get_user_test(Config) ->
    User1 = ?config(row,Config),
    User2 = directory_server:get_user(User1#ibo_user.username),
    io:format("User1: ~p~n",[User1]),
    io:format("User2: ~p~n",[User2]),
    true = User1 =:= User2,
    ok.

test2(_Config) ->
    A = 0,
%%    1/A.
    A = A.