%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     Helper module to access the database
%%% @end
%%% Created : 31. Dez 2015 15:16
%%%-------------------------------------------------------------------
-module(db).
-author("Florian").

%% API
-export([read_transactional/2, write_transactional/1, is_key_in_table/2]).

read_transactional(Table, Key) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:read(Table, Key)
        end),
    case Res of
        {atomic, [Record]} -> Record;
        {atomic, []} -> not_found;
        _ -> {error, "Read failure"}
    end.

write_transactional(Record) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:write(Record)
        end),
    case Res of
        {atomic, ok} -> ok;
        _ -> {error, "Write failure"}
    end.

is_key_in_table(Table, Key) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:read(Table, Key)
        end),
    case Res of
        {atomic, [_]} -> true;
        {atomic, []} -> false;
        _ -> throw("Cannot check if Key is in Table")
    end.