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
    type :: undefined | self | other | group,
    ibo_user :: #ibo_user{} | undefined,
    directory_server_name :: binary()
}).

%% API ---------------------------------------------------------------
-export([init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([to_json/2]).

init(Req, Opts) ->
    Directory = maps:get(directory, Opts),
    {cowboy_rest, Req, #state{directory_server_name = Directory}}.

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
        _Else ->
            cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"type of directory request has to be user or group\"}">>, Req),
            {stop, Req, State}
    end.

%% ------------------------

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

to_json(Req, State) when State#state.type =:= self ->
    Body = jsx:encode(?record_to_tuplelist(ibo_user,State#state.ibo_user)),
    {Body, Req, State};
to_json(Req, State) when State#state.type =:= group ->
    Groups = directory_server:get_all_groups(State#state.directory_server_name),
    PreparedGroups = lists:map(fun(Element) -> ?record_to_tuplelist(ibo_group, Element) end, Groups),
    Body = jsx:encode(PreparedGroups),
    {Body, Req, State}.