%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Dez 2015 12:41
%%%-------------------------------------------------------------------
-module(xlib_box).
-author("Florian").

-include("xlib_state.hrl").

%% API
-export([webinit/1]).

-spec webinit(State :: #xlib_state{}) -> {Group :: nonempty_string(), Taskdetails :: any()}.

% Step which initializes the web interface for the users
webinit(State) ->
    1 = State#xlib_state.current_linenr,
    Step = xlib:get_current_step(State),
    Group = Step#ibo_xbostep.local,
    [Taskdetails|_] = (lists:nth(1, Step#ibo_xbostep.commands))#ibo_xboline.args,

    {Group, Taskdetails}.
