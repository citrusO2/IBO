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
    add_records_to_mnesia/1,
    remove_record_from_mnesia/1,
    traverse_table_and_show/1,
    print_var/2,
    init_mnesia/0,
    deinit_mnesia/0,
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

add_records_to_mnesia([]) -> ok;
add_records_to_mnesia([Record|Records]) ->
    add_record_to_mnesia(Record),
    add_records_to_mnesia(Records).

remove_record_from_mnesia(Record) ->
    F = fun() ->
        mnesia:delete({element(1, Record), element(2, Record)})
        end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

traverse_table_and_show(Table_name)->
    Iterator =  fun(Rec,_)->
        io:format("~p~n",[Rec]),
        []
                end,
    case mnesia:is_transaction() of
        true -> mnesia:foldl(Iterator,[],Table_name);
        false ->
            Exec = fun({Fun,Tab}) -> mnesia:foldl(Fun, [],Tab) end,
            mnesia:activity(transaction,Exec,[{Iterator,Table_name}],mnesia_frag)
    end.

print_var(VarName, Var) ->
    io:format("~p: ~p~n", [VarName, Var]),
    ok.

init_mnesia() ->
    Nodes = [node()],
    ok = mnesia:create_schema(Nodes),
    mnesia:start().

deinit_mnesia() ->
    Nodes = [node()],
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok = mnesia:delete_schema(Nodes).

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
    mnesia:table_info(Table, size).

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