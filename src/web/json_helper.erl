%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     Helper to gracefully prepare records for conversion to json
%%% @end
%%% Created : 06. Dez 2015 22:51
%%%-------------------------------------------------------------------
-module(json_helper).
-author("Florian").

-include("handler_macros.hrl").
-include("../box/box_records.hrl").

%% API ---------------------------------------------------------------
-export([prepare/1, prepareXactors/1]).

prepare(Ref) when is_record(Ref, ibo_boxindex) ->
    SubR = [?record_to_tuplelist(ibo_boxindex_elementpreview,
        E#ibo_boxindex_elementpreview{
            storedate = calendar:now_to_local_time(E#ibo_boxindex_elementpreview.storedate)
        }) || E <- Ref#ibo_boxindex.xbolist ],
    R1 = Ref#ibo_boxindex{xbolist = SubR},
    ?record_to_tuplelist(ibo_boxindex, R1).

prepareXactors(Xactors) when is_list(Xactors) ->
    lists:map( fun(Xactor) -> prepareXactors(Xactor) end, Xactors );
prepareXactors(Xactor) when is_tuple(Xactor) ->
    #{
        name => element(1, Xactor),
        type => element(2, Xactor),
        %args => element(3, Xactor), %start args, skipped on purpose
        info => element(4, Xactor)
    }.