%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jän 2016 06:49
%%%-------------------------------------------------------------------
-author("Florian").

-include("../xlib/xlib_state.hrl").

-ifndef(ERROR_RECORDS_HRL).
-define(ERROR_RECORDS_HRL, 1).

-record(ibo_errordata, {
    xboid :: nonempty_string(),
    xlibstate :: #xlib_state{},
    error :: any(),
    destination :: binary()     % the destination of where the XBO should have been send
}).

-endif. % ERROR_RECORDS_HRL defined