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
-include("handler_macros.hrl").

-record(state, {
    type :: undefined | list | start,
    ibo_user :: #ibo_user{} | undefined,
    templates :: list(nonempty_string())
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
    {cowboy_rest, Req, Opts}.

%% Allowed Methods ---------------------------------------------------
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

%% Authentication ----------------------------------------------------
is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, UserID, Password} ->
            case directory_server:get_user_info(UserID,Password) of
                User when is_tuple(User) andalso element(1,User) =:= ibo_user ->
                    {true, Req, #state{ibo_user = User}};
                _ ->
                    {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
            end;
        _ ->
            {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
    end.

forbidden(Req, State) ->
    case cowboy_req:binding(repo_path, Req) of
        undefined ->    % = no additional path given
            {false, Req, State#state{type = list, templates = repo_server:get_templatelist(State#state.ibo_user)}};
        TemplateName ->
            % TODO: only check if forbidden first, handle other errors later
            case repo_server:start_template(State#state.ibo_user, TemplateName) of
                {error, _Message} ->
                    {true, Req, State#state{type = start}};
                _Else ->
                    {false, Req, State#state{type = start}}
            end
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
    {Body, Req, State}.

json_post(Req, State) when State#state.type =:= start ->
    {ok, _Body, Req2} = cowboy_req:body(Req),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], <<"{\"success\": \"template started\"}">>, Req),
    {true, Req2, State}.