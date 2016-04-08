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

-record(ibo_errorcase, {
    xlibstate :: #xlib_state{},
    error :: any(),
    destination :: binary()     % the destination of where the XBO should have been send
}).

-record(ibo_errordata, {
    xboid :: nonempty_string(),
    cases :: nonempty_list(#ibo_errorcase{})   % it is possible to have the same XBO traveling at the same time but throwing a different error
}).

-endif. % ERROR_RECORDS_HRL defined