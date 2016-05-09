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
-export([init/1]).

% Step which initializes the web interface for the users
-spec init(State :: #xlib_state{}) -> { Group :: binary(), Taskdetails :: #{} }.
init(State) ->
    1 = State#xlib_state.current_linenr,
    Step = xlib:get_current_step(State),
    Group = Step#ibo_xbostep.local,
    Args =  (lists:nth(1, Step#ibo_xbostep.commands))#ibo_xboline.args,

    case length(Args) of
        1 ->
            [Taskdetails|_] = Args;
        2 ->
            [Taskdetailstemplate|_] = Args,
            TransformFun = lists:nth(2, Args),  % Function which transforms the Taskdetailstemplate into the actual taskdetails
            Taskdetails = TransformFun(Taskdetailstemplate, State#xlib_state.xbo#ibo_xbo.stepdata)
    end,
    {Group, Taskdetails}.
