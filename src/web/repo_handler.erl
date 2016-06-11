%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mär 2016 20:13
%%%-------------------------------------------------------------------
-module(repo_handler).
-author("Florian").

-include("../directory/directory_records.hrl").
-include("../repo/repo_records.hrl").
-include("handler_macros.hrl").

-record(state, {
    type :: undefined | list | start | domain | store,
    ibo_user :: #ibo_user{} | undefined,
    templates :: list(nonempty_string()),
    repo_server_name :: binary(),
    directory_server_name :: binary()
}).

%% API ---------------------------------------------------------------
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([json_get/2]).
-export([json_post/2]).

init(Req, Opts) ->
    Repo = maps:get(repo, Opts),
    Directory = maps:get(directory, Opts),
    {cowboy_rest, Req, #state{repo_server_name = Repo, directory_server_name = Directory}}.

%% Allowed Methods ---------------------------------------------------
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

%% Authentication ----------------------------------------------------
is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, UserID, Password} ->
            case directory_server:get_user_info(State#state.directory_server_name, UserID,Password) of
                User when is_tuple(User) andalso element(1,User) =:= ibo_user ->
                    {true, Req, State#state{ibo_user = User}};
                _ ->
                    {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
            end;
        _ ->
            {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
    end.

forbidden(Req, State) ->
    case cowboy_req:binding(repo_type, Req) of
        <<"process">> ->
            case cowboy_req:binding(repo_path, Req) of
                undefined ->    % = no additional path given
                    {false, Req, State#state{type = list, templates = repo_server:get_templatelist(State#state.repo_server_name, State#state.ibo_user#ibo_user.access_to)}};
                TemplateName ->
                    % TODO: only check if forbidden first, handle other errors later
                    case repo_server:start_template(State#state.repo_server_name, State#state.ibo_user#ibo_user.access_to, State#state.ibo_user#ibo_user.username, TemplateName) of
                        {error, _Message} ->
                            {true, Req, State#state{type = start}};
                        _Else ->
                            {false, Req, State#state{type = start}}
                    end
            end;
        <<"domain">> ->
            {false, Req, State#state{type = domain}};
        <<"template">> ->
            case cowboy_req:binding(repo_path, Req) of
                undefined ->    % = no additional path given
                    {false, Req, State#state{type = store}};    % accessing /template without path is forbidden
                TemplateName -> % TODO: only check if forbidden first here, do not save it immediately
                    Body = cowboy_req:body(Req),
                    TemplateMap = jsx:decode(Body, [return_maps]),
                    TemplateRecord = helper:map_to_record_strict(TemplateMap, record_info(fields, ibo_repo_template), ibo_repo_template),
                    TemplateName = TemplateRecord#ibo_repo_template.name,   % testcheck
                    case repo_server:store_template(State#state.repo_server_name, TemplateRecord, State#state.ibo_user#ibo_user.access_to) of
                        {error, _Message} ->
                            {true, Req, State#state{type = store}};
                        _Else ->
                            {false, Req, State#state{type = store}}
                    end
            end;

        _Else ->
%%            io:format("Error of repo_type ~p~n", [Else]),
%%            {true, Req, State#state{type = error}}
            cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"type of repo request has to be process or domain\"}">>, Req),
            {stop, Req, State}
    end.



%% ------------------------

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, json_get}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, '*'}, json_post}
    ], Req, State}.

json_get(Req, State) when State#state.type =:= list ->
    Body = jsx:encode(State#state.templates),
    {Body, Req, State};
json_get(Req, State) when State#state.type =:= domain ->
    Body = jsx:encode(json_helper:prepareXactors(watchdog_server:get_global_xactors())),
    {Body, Req, State}.

json_post(Req, State) when State#state.type =:= store ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], <<"{\"success\": \"template stored\"}">>, Req),
    {true, Req, State};
json_post(Req, State) when State#state.type =:= start ->
    {ok, _Body, Req2} = cowboy_req:body(Req),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], <<"{\"success\": \"template started\"}">>, Req),
    {true, Req2, State}.
