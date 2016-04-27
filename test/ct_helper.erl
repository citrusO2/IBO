%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     test helper file which gets included in other tests
%%% @end
%%% Created : 25. Nov 2015 09:28
%%%-------------------------------------------------------------------
-module(ct_helper).
-author("Florian").

%% API ---------------------------------------------------------------
-export([add_record_to_mnesia/1,
    remove_record_from_mnesia/1,
    print_var/2,
    create_table_for_record/3,
    read_transactional/2,
    get_recordcount_in_table/1,
    wait/1,wait/0,waitms/1,
    is_registered_global/1,
    is_registered_local/1]).

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

print_var(VarName, Var) ->
    io:format("~p: ~p~n", [VarName, Var]),
    ok.

create_table_for_record(Table, RecordInfo, Nodes) ->
    mnesia:create_table(Table,
        [{attributes, RecordInfo},
            {disc_copies, Nodes},
            {type, set}]).

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

get_recordcount_in_table(Table) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:all_keys(Table)
        end),
    case Res of
        {atomic, Record} -> length(Record);
        _ -> {error, "Read failure"}
    end.

% default wait period
wait() ->
    waitms(50).

wait(Sec) ->
    receive
    after (1000 * Sec) -> ok
    end.

waitms(Msecs) ->
    receive
    after (Msecs) -> ok
    end.

is_registered_global(Name) ->
    case global:whereis_name(Name) of
        undefined ->
            false;
        _Pid ->
            true
    end.

is_registered_local(Name) ->
    case whereis(Name) of
        undefined ->
            false;
        _Pid ->
            true
    end.