%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Dez 2015 21:16
%%%-------------------------------------------------------------------
-author("Florian").

-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec), tl(tuple_to_list(Ref)))).

-define(record_to_map(Rec, Ref), maps:from_list(?record_to_tuplelist(Rec,Ref))).