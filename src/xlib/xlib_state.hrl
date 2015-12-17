%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Dez 2015 12:44
%%%-------------------------------------------------------------------
-author("Florian").

-include("../box/box_records.hrl").

-record(xlib_state, {
    xbo :: #ibo_xbo{},
    current_linenr :: non_neg_integer(),
    current_stepdata :: #ibo_xbostepdata{},
    ttl = 500 :: non_neg_integer()          % maximum possible tail recursive function calls
}).