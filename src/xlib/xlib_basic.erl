%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. Mai 2016 17:09
%%%-------------------------------------------------------------------
-module(xlib_basic).
-author("Florian").

-include("xlib_state.hrl").

%% API
-export([cjump/3]).
-export([send/3]).
-export([finish/1]).

% conditional jump to a certain line
-spec cjump(#xlib_state{},non_neg_integer(),fun((#ibo_xbostepdata{},list(#ibo_xbostepdata{})|[]) -> true|false)) -> any() .
cjump(State, LineNr, Condition) ->
    case Condition(State#xlib_state.current_stepdata, State#xlib_state.xbo#ibo_xbo.stepdata) of
        true ->
            xlib:next(State#xlib_state{current_linenr = LineNr - 1});    % -1 because next increases the LineNr by 1
        false ->
            xlib:next(State)
    end.

% ends the execution of the step and sends it to another destination
send(State, NewStepNr, NewDestination) ->
    {send, State, NewStepNr, NewDestination}.

% ends the execution and finishes the xbo, by sending it to the router for the last time
finish(State) ->
    {finish, State}.