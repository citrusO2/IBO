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
    %io:format("taskdetails: (~p)~n", [Taskdetails]),
    NewTaskDetails = init_dynamic_description(Taskdetails, State#xlib_state.xbo#ibo_xbo.stepdata),
    {Group, NewTaskDetails}.

%%%===================================================================
%%% internal functions
%%%===================================================================

% makes the description of the given schema dynamic
% it adds the current to the placeholder field: [StepNr|MyVariable] -> [StepNr|MyVariable|Value]
init_dynamic_description(Taskdetails, Stepdata) ->
    case maps:is_key(<<"description">>, Taskdetails) of
        false -> Taskdetails;
        true ->
            OldDescription = maps:get(<<"description">>, Taskdetails),
            case re:run(OldDescription,"\\[(.*?)\\]", [global]) of  % gets all elements between [ ],
                {match, Captured} ->
                    NewDescription = update_description(Captured, OldDescription, 0, Stepdata),
                    maps:update(<<"description">>, NewDescription, Taskdetails);
                _Else -> Taskdetails % nothing found
            end
    end.

%% iterates over the description and tries to add the actual value of the variable
%% offset is the difference of the starting position caused by inserting additional data
% example captured when using [global] in "re:run": {match,[[{32,9},{33,7}],[{54,9},{55,7}]]}
% example match: [{32,9},{33,7}] -> the first element is with brackets included, the second one without
update_description([], Description, _Offset, _Stepdata) ->
    Description;
update_description([Match|Captured], Description, Offset, Stepdata) ->
    %io:format("Match: (~p)~n", [Match]),
    {Start, Length} = lists:nth(2, Match),
    OldVariableTemplate = binary:part(Description, Start + Offset, Length),


    BinaryToInsert = try
        [StepNr,VariableName|_Rest] = binary:split(OldVariableTemplate, <<"|">>),   % contains Rest, because of possible future extension with e.g. formatting
        % get variable to insert
        case xlib_helper:get_latest_variable(Stepdata, binary_to_integer(StepNr),VariableName) of
            {ok, <<"">>} -> <<"variable empty">>;
            {ok, Value} -> any_to_binary(Value);
            error -> <<"variable not found">>
        end
    catch
        _Exception:_Reason -> <<"variable error">>
    end,


    % insert variable at right position
    CurrentPositionToInsert = Start + Length + Offset,
    <<P1:CurrentPositionToInsert/binary,P2/binary>> = Description,
    NewDescription = binary:list_to_bin([P1,<<"|">>,BinaryToInsert,P2]),

    % update offset, so that the newly inserted data does not mess up start and length from the variable Match
    NewOffset = Offset + erlang:size(NewDescription) - erlang:size(Description),
    update_description(Captured, NewDescription, NewOffset,Stepdata).


any_to_binary(Any) when is_binary(Any)->
    Any;
any_to_binary(Any) when is_atom(Any) ->
    any_to_binary(atom_to_list(Any));
any_to_binary(Any) when is_list(Any) ->
    list_to_binary(Any);
any_to_binary(Any) when is_integer(Any) ->
    integer_to_binary(Any);
any_to_binary(Any) when is_float(Any)->
    float_to_binary(Any).