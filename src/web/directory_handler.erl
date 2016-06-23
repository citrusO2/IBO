%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Dez 2015 21:14
%%%-------------------------------------------------------------------
-module(directory_handler).
-author("Florian").

-include("../directory/directory_records.hrl").
-include("handler_macros.hrl").

-record(state, {
    type :: undefined | self | other | group | logout,
    ibo_user :: #ibo_user{} | undefined,
    directory_server_name :: binary(),
    web_server_name :: binary()
}).

%% API ---------------------------------------------------------------
-export([init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([json_get/2]).

init(Req, Opts) ->
    Directory = maps:get(directory, Opts),
    WebServer = maps:get(name, Opts),
    {cowboy_rest, Req, #state{directory_server_name = Directory, web_server_name = WebServer}}.

%%%===================================================================
%%% General functions
%%%===================================================================
content_types_provided(Req, State) ->
    general_handler:content_types_provided(Req, State).

%%content_types_accepted(Req, State) -> % not used as there are no post-requests to the directory
%%    general_handler:content_types_accepted(Req, State).

is_authorized(Req, State) ->
    general_handler:is_authorized(Req, State, State#state.directory_server_name, State#state.web_server_name).

%%%===================================================================
%%% Handler implementations
%%%===================================================================
forbidden(Req, State) ->
    case cowboy_req:binding(dir_type, Req) of
        <<"user">> ->
            case cowboy_req:binding(box_path, Req) of
                undefined ->    % = no additional path given
                    {false, Req, State#state{type = self}};
                _UserID ->
                    {true, Req, State#state{type = other}} % TODO: implement retrieving other user
                % check if the user is allowed to retrieve other users
                % if not found -> send err404 and return {stop,Req,State}
                % if no access -> return false, otherwise true
            end;
        <<"group">> ->
            {false, Req, State#state{type = group}};
        <<"logout">> ->
            {false, Req, State#state{type = logout}};
        _Else ->
            cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"type of directory request has to be user, group or logout\"}">>, Req),
            {stop, Req, State}
    end.

%% ------------------------

json_get(Req, State) when State#state.type =:= logout ->
    SessionKey = get_session_key(Req),
    Body = case web_server:clear_user_cache(State#state.web_server_name, State#state.ibo_user#ibo_user.username, SessionKey) of
               ok -> <<"\"logout successful\"">>;
               _Else -> <<"\"logout incomplete\"">>
           end,
    {Body, Req, State};
json_get(Req, State) when State#state.type =:= self ->
    Body = jsx:encode(?record_to_tuplelist(ibo_user,State#state.ibo_user)),
    {Body, Req, State};
json_get(Req, State) when State#state.type =:= group ->
    Groups = directory_server:get_all_groups(State#state.directory_server_name),
    PreparedGroups = lists:map(fun(Element) -> ?record_to_tuplelist(ibo_group, Element) end, Groups),
    Body = jsx:encode(PreparedGroups),
    {Body, Req, State}.

%%%===================================================================
%%% Helper
%%%===================================================================
get_session_key(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(<<"sessionid">>, 1, Cookies) of
        {_, SessionId} -> SessionId
    end.