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

-export([xlib_info/0]).

xlib_info() ->
    #{
        init => #{
            description => <<"Initializes the user's input window for the given step">>,
            args => [
                #{
                    name => <<"Schema">>,
                    description => <<"The schema for initialization">>,
                    type => <<"schema">>   % work in progress
                }
            ]
        }
    }.

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
        2 ->    % possible to use a transformation function for the second argument (only possible when template is defined via erlang code)
            [Taskdetailstemplate|_] = Args,
            TransformFun = lists:nth(2, Args),  % Function which transforms the Taskdetailstemplate into the actual taskdetails
            Taskdetails = TransformFun(Taskdetailstemplate, State#xlib_state.xbo#ibo_xbo.stepdata)
    end,
    {Group, Taskdetails}.
