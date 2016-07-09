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
    template_name :: binary(),  % only in step repo_type template & case store
    repo_server_name :: binary(),
    directory_server_name :: binary(),
    web_server_name :: binary()
}).

%% API ---------------------------------------------------------------
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([delete_resource/2]).
-export([json_get/2]).
-export([json_post/2]).

init(Req, Opts) ->
    Repo = maps:get(repo, Opts),
    Directory = maps:get(directory, Opts),
    WebServer = maps:get(name, Opts),
    {cowboy_rest, Req, #state{repo_server_name = Repo, directory_server_name = Directory, web_server_name = WebServer}}.

%% Allowed Methods ---------------------------------------------------
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>, <<"DELETE">>], Req, State}.

%%%===================================================================
%%% General functions
%%%===================================================================
content_types_provided(Req, State) ->
    general_handler:content_types_provided(Req, State).

content_types_accepted(Req, State) ->
    general_handler:content_types_accepted(Req, State).

is_authorized(Req, State) ->
    general_handler:is_authorized(Req, State, State#state.directory_server_name, State#state.web_server_name).

%%%===================================================================
%%% Handler implementations
%%%===================================================================
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
                    {true, Req, State#state{type = store}};    % accessing /template without path is forbidden
                TemplateName ->
                    Forbidden = not repo_server:has_store_permission(State#state.repo_server_name, State#state.ibo_user),
                    {Forbidden, Req, State#state{type = store, template_name = TemplateName}}
            end;

        _Else ->
%%            io:format("Error of repo_type ~p~n", [Else]),
%%            {true, Req, State#state{type = error}}
            cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"type of repo request has to be process, domain or template\"}">>, Req),
            {stop, Req, State}
    end.

delete_resource(Req, State) when State#state.type =:= store ->
    case repo_server:delete_template(State#state.repo_server_name, State#state.template_name, State#state.ibo_user) of
        ok ->
            {true, Req, State};
        not_found ->
            cowboy_req:reply(404,[{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"template could not be found\"}">>, Req),
            {stop, Req, State};
        {error, _Message} ->
            cowboy_req:reply(500,[{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"error deleting template\"}">>, Req),
            {stop, Req, State}
    end.

json_get(Req, State) when State#state.type =:= store ->
    case repo_server:get_template(State#state.repo_server_name, State#state.template_name) of
        not_found ->
            cowboy_req:reply(404,[{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"template could not be found\"}">>, Req),
            {stop, Req, State};
        {error, _Message} ->
            cowboy_req:reply(500,[{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"error retrieving template\"}">>, Req),
            {stop, Req, State};
        Template ->
            Body = jsx:encode( json_helper:prepare(Template)),
            {Body, Req, State}
    end;
json_get(Req, State) when State#state.type =:= list ->
    Body = jsx:encode( lists:map(fun(Template) -> #{name => element(1, Template), description => element(2, Template)} end,State#state.templates)),
    {Body, Req, State};
json_get(Req, State) when State#state.type =:= domain ->
    Body = jsx:encode(json_helper:prepareXactors(watchdog_server:get_global_xactors())),
    {Body, Req, State}.

json_post(Req, State) when State#state.type =:= store ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    TemplateMap = jsx:decode(Body, [return_maps]),
    TemplateRecord = json_helper:prepareTemplate(TemplateMap, State#state.ibo_user#ibo_user.username),% helper:map_to_record_strict(TemplateMap, record_info(fields, ibo_repo_template), ibo_repo_template),
    if
        State#state.template_name == TemplateRecord#ibo_repo_template.name ->
            case json_helper:checkTemplate(TemplateRecord) of
                {error, Reason} ->
                    io:format("Error storing template, reason: ~p~n", [Reason]),
                    cowboy_req:reply(422, [{<<"content-type">>, <<"application/json">>}], list_to_binary([<<"{\"error\": \"template could not be stored, error in template: ">>,list_to_binary(Reason), <<"\"}">>]), Req2);
                true ->
                    case repo_server:store_template(State#state.repo_server_name, TemplateRecord, State#state.ibo_user#ibo_user.access_to) of
                        {error, Message} ->
                            io:format("Error storing template, message: ~p~n", [Message]),
                            cowboy_req:reply(422, [{<<"content-type">>, <<"application/json">>}], list_to_binary([<<"{\"error\": \"template could not be stored, error in template: ">>,list_to_binary(Message), <<"\"}">>]), Req2);
                        _Else ->
                            cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], <<"{\"success\": \"template stored successfully\"}">>, Req2)
                    end
            end;
        State#state.template_name /= TemplateRecord#ibo_repo_template.name ->
            io:format("Error, template_name url and template name do not match~n"),
            cowboy_req:reply(422, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"template could not be stored, error in template\"}">>, Req2)
    end,
    {true, Req2, State};

json_post(Req, State) when State#state.type =:= start ->
    {ok, _Body, Req2} = cowboy_req:body(Req),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], <<"{\"success\": \"template started\"}">>, Req),
    {true, Req2, State}.
