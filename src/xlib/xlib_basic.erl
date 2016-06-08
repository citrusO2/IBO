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

-export([xlib_info/0]).

% returns a map with the exported lib-functions
xlib_info() ->
    #{
        cjump => #{
            description => <<"Manipulates the order of the command execution">>,
            args => [
                #{
                    name => <<"LineNr">>,
                    description => <<"The line number that the execution should jump to">>,
                    type => <<"integer">>   % work in progress
                },
                #{
                    name => <<"Condition">>,
                    description => <<"The condition used to evaluate if the execution should jump to the given line number">>,
                    type => <<"condition">>
                }
            ]
        },
        send => #{
            description => <<"Sends the XBO to the next actor">>,
            args => [
                #{
                    name => <<"NewStepNr">>,
                    description => <<"The next step to execute">>,
                    type => <<"step">> % work in progress
                }%,
%%                #{
%%                    name => <<"Destination">>,
%%                    description => <<"The destination that the packet should be send to">>,
%%                    type => <<"domain">> % work in progress
%%                }
            ]
        },
        finish => #{
            description => <<"Ends the execution of the XBO">>,
            args => []
        }

    }.

% conditional jump to a certain line
-spec cjump(#xlib_state{},non_neg_integer(),fun((#ibo_xbostepdata{},list(#ibo_xbostepdata{})|[]) -> true|false)) -> any() .
cjump(State, LineNr, Condition) when is_function(Condition, 2) ->
    case Condition(State#xlib_state.current_stepdata, State#xlib_state.xbo#ibo_xbo.stepdata) of
        true ->
            xlib:next(State#xlib_state{current_linenr = LineNr - 1});    % -1 because next increases the LineNr by 1
        false ->
            xlib:next(State)
    end;

% conditional jump with the condition given as a list
cjump(State, LineNr, Condition) when is_list(Condition) ->
    case xcondition:eval(Condition, State) of
        error ->
            {error, State, "xconditon evaluation error"};
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