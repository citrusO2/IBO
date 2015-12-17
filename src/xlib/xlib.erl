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
-export([cjump/3]).
-export([send/3]).
-export([finish/1]).
-export([get_current_command/1, get_current_step/1]).

start(State) ->
    apply_state(State).

% conditional jump to a certain line
-spec cjump(#xlib_state{},non_neg_integer(),fun((#ibo_xbostepdata{},list(#ibo_xbostepdata{})|[]) -> true|false)) -> any() .
cjump(State, LineNr, Condition) ->
    case Condition(State#xlib_state.current_stepdata, State#xlib_state.xbo#ibo_xbo.stepdata) of
        true ->
            next(State#xlib_state{current_linenr = LineNr - 1});    % -1 because next increases the LineNr by 1
        false ->
            next(State)
    end.

% ends the execution of the step and sends it to another destination
send(State, NewStepNr, NewDestination) ->
    {send, State, NewStepNr, NewDestination}.

% ends the execution and finishes the xbo, by sending it to the router for the last time
finish(State) ->
    {finish, State}.

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

