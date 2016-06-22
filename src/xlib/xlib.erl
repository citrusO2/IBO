%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     general execution library
%%% @end
%%% Created : 07. Dez 2015 12:43
%%%-------------------------------------------------------------------
-module(xlib).
-author("Florian").

-include("xlib_state.hrl").

%% API
-export([start/1]).

-export([get_libs/0]).

-export([next/1, get_current_command/1, get_current_step/1]).

%% function which returns all available libs and commands
get_libs() ->
    #{
        xlib_basic => xlib_basic:xlib_info(),
        xlib_box => xlib_box:xlib_info()
    }.

%% function to start execution of an xlib_state
start(State) ->
    apply_state(State).

% called at the end of each step to continue the loop
next(State) when State#xlib_state.ttl > 0 ->
    TTL = State#xlib_state.ttl - 1,
    CurrentLineNr = State#xlib_state.current_linenr + 1,
    NewState = State#xlib_state{ttl = TTL, current_linenr = CurrentLineNr},
    apply_state(NewState);
next(State) when State#xlib_state.ttl =< 0 ->
    throw(ttl_end).

%%%===================================================================
%%% Helper
%%%===================================================================
get_current_step(State) ->
    lists:nth(State#xlib_state.current_stepdata#ibo_xbostepdata.stepnr,
        State#xlib_state.xbo#ibo_xbo.steps).

get_current_command(State)->
    lists:nth(State#xlib_state.current_linenr, (get_current_step(State))#ibo_xbostep.commands).

apply_state(State) ->
    Command = get_current_command(State),
    if
        Command#ibo_xboline.args =:= undefined ->
            erlang:apply(Command#ibo_xboline.library, Command#ibo_xboline.command, [State]);
        true ->
            erlang:apply(Command#ibo_xboline.library, Command#ibo_xboline.command, [State|Command#ibo_xboline.args])
    end.

