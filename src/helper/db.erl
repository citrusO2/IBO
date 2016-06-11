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
-export([read_transactional/2, write_transactional/1, delete_transactional/2, is_key_in_table/2, is_table_existing/1, create_local_table_if_nonexistent/4]).

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

delete_transactional(Table, Key) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:delete(Table, Key, write)
        end),
    case Res of
        {atomic, ok} -> ok;
        _ -> {error, "Delete failure"}
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

is_table_existing(Table) ->
    Tables = mnesia:system_info(tables),
    lists:member(Table,Tables).

% creates a local table with the given parameters if the table does not exist yet
create_local_table_if_nonexistent(TableName, RecordInfo, Replication, Type)->
    create_local_table_if_nonexistent(TableName, RecordInfo, Replication, Type, is_table_existing(TableName)).

create_local_table_if_nonexistent(_TableName, _RecordInfo, _Replication, _Type, true) -> % table does exist, nothing to do
    ok;
create_local_table_if_nonexistent(TableName, RecordInfo, Replication, Type, false) ->           % table does not exist, create it
    {atomic, ok} = mnesia:create_table(TableName,
        [{attributes, RecordInfo },
            {Replication, [node()]},
            {type, Type}]),
    ok.