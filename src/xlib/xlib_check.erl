%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     Module to check if the xbo's are correct
%%% @end
%%% Created : 22. Jun 2016 18:49
%%%-------------------------------------------------------------------
-module(xlib_check).
-author("Florian").

-include("xlib_state.hrl").

%% API
-export([check_repo_template_steps/1, check_xbo_step/4]).

%% checks if the given steps for the repository are valid
check_repo_template_steps(Steps) ->
    try check_repo_template_steps_throw(Steps) of
        ok -> ok
    catch
        _:Error -> {error, Error}
    end.

check_repo_template_steps_throw(Steps) ->
    lists:foreach(fun(Step) ->
        XlibInfo = get_xlibinfo_by_domainname(Step#ibo_xbostep.domain), % get possible commands from the running xactor (xactor must implement call with 'xlib_info')
        check_step_throw(Step, XlibInfo)
        end, Steps),
    ok.

%% function to check if the XlibInfo matches the XBO, StepNr and Domain, but does not check the init function itself if one is used
check_xbo_step(XBO, StepNr, Domain, XlibInfo) ->
    try check_xbo_step_throw(XBO, StepNr, Domain, XlibInfo) of
        ok -> ok
    catch
        _:Error -> {error, Error}
    end.
%%    case check_xbo_step_throw(XBO, StepNr, Domain, XlibInfo) of
%%        ok -> ok
%%    end.

%% stops the execution of the checks as soon as the first error is detected
check_xbo_step_throw(XBO, StepNr, Domain, XlibInfo) ->
    %% check XBO domain info
    StepCount = length(XBO#ibo_xbo.steps),  %check if stepnr is higher than the amount of steps
    throw_if_true(StepNr > StepCount, "StepNr outside StepRange"),
    Step = lists:nth(StepNr, XBO#ibo_xbo.steps),    % check if the step is for the given domain
    throw_if_false(Step#ibo_xbostep.domain =:= Domain, "Step is for a different domain"),
    XBOid = XBO#ibo_xbo.id,     % check if the XBO even has an ID
    throw_if_true(XBOid =:= "", "Id of XBO must not be empty"),

    % check the step itself
    check_step_throw(Step, XlibInfo).

%% function to check the given step
check_step_throw(Step, XlibInfo) ->
    CommandCount = length(Step#ibo_xbostep.commands),
    throw_if_false(CommandCount > 0, "There must be at least one command in the step"),

    %% check if the XBO Step requires an init or not
    LibsMap = maps:get(libraries, XlibInfo),
    FirstLine = lists:nth(1, Step#ibo_xbostep.commands),
    OtherCommands = case maps:find(init, XlibInfo) of
                        {ok, Library} ->
                            throw_if_false((FirstLine#ibo_xboline.library =:= Library andalso FirstLine#ibo_xboline.command =:= init), "The first command must be init from the library " ++ atom_to_list(Library)),
                            throw_if_false( function_exported(Library,init), "The library " ++ atom_to_list(Library) ++ " does not contain an init command"),
                            case check_xbo_command_arguments(FirstLine, LibsMap) of
                                ok -> ok;
                                {error, ErroMsg} -> throw(ErroMsg)
                            end,
                            tl(Step#ibo_xbostep.commands);
                        error ->
                            throw_if_true( FirstLine#ibo_xboline.command =:= init, "The first command must not be init"),
                            Step#ibo_xbostep.commands
                    end,

    %% check if only certain libraries are used and if the used libraries even contain the right function (with the right arity)
    Libraries = maps:keys(LibsMap),
    lists:foreach(fun(Line = #ibo_xboline{library = L, command = C, args = A}) ->
        throw_if_false(lists:member(L, Libraries), "The library " ++ atom_to_list(L) ++ " cannot be used"),
        if
            C =:= init -> % additional checks of init might be necessary by the actor using the particular init function
                throw("Only the first command can be an init command");
            A =:= undefined ->  % the command does not require any additional arguments
                throw_if_false( function_exported(L,C,1), "The library " ++ atom_to_list(L) ++ " does not contain the command " ++ atom_to_list(C));
            is_list(A) ->   % Arity + 1 because internally the state is also added to the command
                throw_if_false( function_exported(L,C,1+length(A)), "The command " ++ atom_to_list(C) ++ " of the library " ++ atom_to_list(L) ++ " does not exist or was used with a wrong argument")
        end,
        case check_xbo_command_arguments(Line, LibsMap) of
            ok -> ok;
            {error, Error} -> throw(Error)
        end end, OtherCommands),
    ok.

% checks the given command line's arguments
-spec check_xbo_command_arguments(#ibo_xboline{}, #{}) -> ok | {error, term()}.
check_xbo_command_arguments(#ibo_xboline{library = L, command = C, args = A}, Xlibs) ->
    SpecifiedCommands = maps:get(L, Xlibs),
    SpecifiedArgTypes = lists:map(fun(El) -> maps:get(type, El) end, maps:get(args, maps:get(C, SpecifiedCommands))),
    check_xbo_arg(SpecifiedArgTypes, A).

check_xbo_arg([],undefined) -> ok;  % no args given
check_xbo_arg([],[]) -> ok; % recursively check all given arguments if they match the specified argument types
check_xbo_arg([<<"integer">> | OtherArgTypes], [GivenArg | OtherGivenArgs]) ->
    if
        is_integer(GivenArg) -> check_xbo_arg(OtherArgTypes, OtherGivenArgs);
        true -> {error, "Argument must be of type integer"}
    end;
check_xbo_arg([<<"line">> | OtherArgTypes], [GivenArg | OtherGivenArgs]) ->
    if
        is_integer(GivenArg), GivenArg > 0 -> check_xbo_arg(OtherArgTypes, OtherGivenArgs);
        true -> {error, "Argument must be of type line and > 0"}    % should also have a maximum, which is the length of commands in the step
    end;
check_xbo_arg([<<"condition">> | OtherArgTypes], [GivenArg | OtherGivenArgs]) ->
    if
        is_function(GivenArg) -> check_xbo_arg(OtherArgTypes, OtherArgTypes);
        is_list(GivenArg), length(GivenArg) == 4 -> check_xbo_arg(OtherArgTypes, OtherGivenArgs);
        true ->  {error, "Argument must be of type condition"}
    end;
check_xbo_arg([<<"step">> | OtherArgTypes], [GivenArg | OtherGivenArgs]) ->
    [DomainName|MoreGivenArgs] = OtherGivenArgs,    %step consists of two arguments
    if
        is_integer(GivenArg), GivenArg > 0, is_binary(DomainName) -> check_xbo_arg(OtherArgTypes, MoreGivenArgs);
        true -> {error, "Argument step must consist of a integer stepnr > 0 and a binary name as the destination"}
    end;
check_xbo_arg([<<"schema">> | OtherArgTypes], [GivenArg | OtherGivenArgs]) ->
    case schema_validator:validate_schema(GivenArg) of
        {ok, _} ->
            if
                is_function(hd(OtherGivenArgs), 2) -> check_xbo_arg(OtherArgTypes, tl(OtherGivenArgs));  % after schema arg, a function can follow that can modify the schema, so if the following argument is a function -> remove it (remove this check if removing that functionality)
                true -> check_xbo_arg(OtherArgTypes, OtherGivenArgs)
            end;
        {error, {ReasonText, _}} ->
            {error, "Argument schema must be a valid schema: " ++ ReasonText}
    end;
check_xbo_arg([ArgumentType| _OtherArgTypes], [_GivenArg | _OtherGivenArgs]) ->
    {error, "Given argument type is not implemented" ++ binary_to_list(ArgumentType)};
check_xbo_arg([SpecifiedArgTypes|_OtherArgTypes], _GivenArgs) when is_binary(SpecifiedArgTypes)->
    io:format("no matching arguments found: (~p)(~p)",[SpecifiedArgTypes,_GivenArgs] ),
    {error, "no matching argument could be found for type: " ++ binary_to_list(SpecifiedArgTypes)};
check_xbo_arg(_SpecifiedArgTypes, _GivenArgs) ->
    io:format("no matching arguments found: (~p)(~p)",[_SpecifiedArgTypes,_GivenArgs] ),
    {error, "no matching argument could be found for the argument check"}.

%%%===================================================================
%%% Helper
%%%===================================================================
get_xlibinfo_by_domainname(DomainName) ->
    gen_server:call({global, DomainName}, xlib_info).

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

