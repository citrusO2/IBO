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
-export([check_xbo/4]).
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

%% function to check if the XlibInfo matches the XBO, StepNr and Domain, but does not check the init function itself if one is used
check_xbo(XBO, StepNr, Domain, XlibInfo) ->
    try check_xbo_throw(XBO, StepNr, Domain, XlibInfo) of
        ok -> ok
    catch
        _:Error ->
            {error, Error}
    end.

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

%% stops the execution of the checks as soon as the first error is detected
check_xbo_throw(XBO, StepNr, Domain, XlibInfo) ->
    %check if stepnr is higher than the amount of steps
    StepCount = length(XBO#ibo_xbo.steps),
    throw_if_true(StepNr > StepCount, "StepNr outside StepRange"),

    % check if the step is for the given domain
    Step = lists:nth(StepNr, XBO#ibo_xbo.steps),
    throw_if_false(Step#ibo_xbostep.domain =:= Domain, "Step is for a different domain"),

    % check if the XBO even has an ID
    XBOid = XBO#ibo_xbo.id,
    throw_if_true(XBOid =:= "", "Id of XBO must not be empty"),

    %% check if the XBO Step requires an init or not
    Line = lists:nth(1, Step#ibo_xbostep.commands),
    OtherCommands = case maps:find(init, XlibInfo) of
        {ok, Library} ->
            throw_if_false((Line#ibo_xboline.library =:= Library andalso Line#ibo_xboline.command =:= init), "The first command must be init from the library " ++ atom_to_list(Library)),
            throw_if_false( function_exported(Library,init), "The library " ++ atom_to_list(Library) ++ " does not contain an init command"),
            tl(Step#ibo_xbostep.commands);
        error ->
            throw_if_true( Line#ibo_xboline.command =:= init, "The first command must not be init"),
            Step#ibo_xbostep.commands
    end,

    %% check if only certain libraries are used and if the used libraries even contain the right function
    Libraries = maps:keys(maps:get(libraries, XlibInfo)),
    lists:foreach(fun(#ibo_xboline{library = L, command = C, args = A}) ->
        throw_if_false(lists:member(L, Libraries), "The library " ++ atom_to_list(L) ++ " cannot be used"),
        if
            C =:= init -> % content of init has to be checked separately by the actor using the particular init
                throw("Only the first command can be an init command");
            A =:= undefined ->
                throw_if_false( function_exported(L,C,1), "The library " ++ atom_to_list(L) ++ " does not contain the command " ++ atom_to_list(C));
            is_list(A) ->   % Arity + 1 because internally the state is also added to the command
                throw_if_false( function_exported(L,C,1+length(A)), "The command " ++ atom_to_list(C) ++ " of the library " ++ atom_to_list(L) ++ " does not exist or was used with a wrong argument")
        end end, OtherCommands),
    ok.

throw_if_false(Expression,ThrowReason) ->
    case Expression of
        true ->
            ok;
        _ ->
            throw(ThrowReason)
    end.
throw_if_true(Expression,ThrowReason)->
    throw_if_false(not Expression,ThrowReason).

function_exported(Module, Function, Arity) -> %% error in erlang, erlang:function_exported does not work without having the function called before or only after calling :module_info() of the module in question -> crashes tests
    ExportedFunctions = apply(Module, module_info,[exports]),
    %io:format("Exported functions: (~p)~n", [ExportedFunctions]),
    lists:any( fun({F,A}) ->
        (F =/= module_info) andalso (F =/= xlib_info) andalso (F =:= Function) and (A =:= Arity)  % module_info gets exported automatically but should never be used as an xlib command
    end, ExportedFunctions).

function_exported(Module, Function) ->
    ExportedFunctions = apply(Module, module_info,[exports]),
    %io:format("Exported functions: (~p)~n", [ExportedFunctions]),
    lists:any( fun({F,_A}) ->
        (F =/= module_info) andalso (F =/= xlib_info) andalso (F =:= Function)    % module_info gets exported automatically but should never be used as an xlib command
    end, ExportedFunctions).

