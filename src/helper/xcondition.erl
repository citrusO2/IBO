%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     This module is used to evaluate conditions in the xlib_library
%%% @end
%%% Created : 06. Jun 2016 22:08
%%%-------------------------------------------------------------------
-module(xcondition).
-author("Florian").

%% API
-export([eval/2, eval/3]).

-include("../xbo/xbo_records.hrl").
-include("../xlib/xlib_state.hrl").

-spec eval(Condition :: list(term()), CurrentStepdata :: #ibo_xbostepdata{}, AllOtherStepdata :: list(#ibo_xbostepdata{})) -> boolean() | error.
eval(Condition, CurrentStepdata, AllOtherStepdata) ->
    eval(Condition, [CurrentStepdata|AllOtherStepdata]).    % first, add the current stepdata to all current available stepdata

-spec eval( Condition :: list(term()), StepdataList :: list(#ibo_xbostepdata{}) | #xlib_state{}) -> boolean() | error.
eval(Condition, #xlib_state{xbo = XBO, current_stepdata = CurrentStepData} = XlibState) when is_record(XlibState, xlib_state) ->
    eval(Condition, CurrentStepData, XBO#ibo_xbo.stepdata);
eval([StepNr, VarName, Operator, Value], StepdataList) when is_list(StepdataList) ->
    case get_latest_variable(StepdataList, StepNr, VarName) of
        error ->
            error;
        {ok, VariableValue} ->
            eval_var(VariableValue, Operator, Value)
    end.

-spec eval_var(VariableValue :: binary() | integer() | number(), Operator :: binary(),
    GivenValue :: binary() | integer() | number()) -> boolean().

eval_var(VariableValue, <<">=">>, GivenValue) ->
    VariableValue >= GivenValue;
eval_var(VariableValue, <<">">>, GivenValue) ->
    VariableValue > GivenValue;
eval_var(VariableValue, <<"<">>, GivenValue) ->
    VariableValue < GivenValue;
eval_var(VariableValue, <<"<=">>, GivenValue) ->
    VariableValue =< GivenValue;
eval_var(VariableValue, <<"equal">>, GivenValue) ->
    VariableValue == GivenValue;
eval_var(VariableValue, <<"unequal">>, GivenValue) ->
    VariableValue /= GivenValue;
eval_var(VariableValue, <<"starts with">>, GivenValue) ->
    case binary:match(VariableValue, GivenValue) of
        nomatch -> false;
        {0, _Length} -> true;
        {_Start, _Length} -> false
    end;
eval_var(VariableValue, <<"ends with">>, GivenValue) ->
    TotalLength = byte_size(VariableValue),
    PatternLength = byte_size(GivenValue),
    case binary:match(VariableValue, GivenValue, [{scope, {TotalLength - PatternLength, PatternLength}} ]) of
        nomatch -> false;
        {_Start, _Length} -> true
    end;
eval_var(VariableValue, <<"contains">>, GivenValue) ->
    case binary:match(VariableValue, GivenValue) of
        nomatch -> false;
        {_Start, _Length} -> true
    end;
eval_var(_,_,_) ->
    error.

get_latest_variable([Stepdata|OtherStepdata], StepNr, VarName) ->
    case Stepdata#ibo_xbostepdata.stepnr =:= StepNr of
        false ->
            get_latest_variable(OtherStepdata, StepNr, VarName);   % not the stepdata we are looking for
        true ->
            case maps:find(VarName, Stepdata#ibo_xbostepdata.vars) of
                {ok, Value} -> {ok, Value};
                error -> get_latest_variable(OtherStepdata, StepNr, VarName)  % in case the variable was not defined in the latest version of the step, but in an earlier version of the step -> usually the variable should already be found before this line
            end
    end;
get_latest_variable([], _,_) ->  % variable could not be found
    error.
