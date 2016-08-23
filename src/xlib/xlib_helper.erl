%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Aug 2016 06:25
%%%-------------------------------------------------------------------
-module(xlib_helper).
-author("Florian").

-include("../xbo/xbo_records.hrl").

%% API
-export([get_latest_variable/3]).

% retrieves the latest variable from the stepdata by stepnr and variable name
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
