%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     test helper file which gets included in other tests
%%% @end
%%% Created : 25. Nov 2015 09:28
%%%-------------------------------------------------------------------

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

create_table_for_record(Table, RecordInfo, Nodes) ->
    mnesia:create_table(Table,
        [{attributes, RecordInfo},
            {disc_copies, Nodes},
            {type, set}]).