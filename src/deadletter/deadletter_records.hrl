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

-ifndef(DEADLETTER_RECORDS_HRL).
-define(DEADLETTER_RECORDS_HRL, 1).

-record(ibo_deadlettercase, {
    xlibstate :: #xlib_state{},
    error :: any()
}).

-record(ibo_deadletterdata, {
    xboid :: nonempty_string(),
    cases :: nonempty_list(#ibo_deadlettercase{})   % it is possible to have the same XBO traveling at the same time but throwing a different error
}).

-endif. % DEADLETTER_RECORDS_HRL defined