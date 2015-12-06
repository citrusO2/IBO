%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dez 2015 14:27
%%%-------------------------------------------------------------------
-module(box_handler).
-author("Florian").

-include("../directory/directory_records.hrl").
-include("../box/box_records.hrl").
-include("handler_macros.hrl").

-record(state, {
    type :: undefined | overview | detail,
    ibo_user :: #ibo_user{} | undefined,
    ibo_boxdata :: #ibo_boxdata{} | undefined,
    ibo_boxindices :: [#ibo_boxindex{}] | undefined
}).

%% API ---------------------------------------------------------------
-export([init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([to_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

%% Authentication ----------------------------------------------------
is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, UserID, Password} ->
            case directory_server:get_user_info(UserID, Password) of
                User when is_tuple(User) andalso element(1, User) =:= ibo_user ->
                    {true, Req, #state{ibo_user = User}};
%%                {error, Reason} ->
%%                    {{false, <<"Basic realm=\"cowboy\"">>}, Req, State};
                _ ->
                    {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
            end;
        _ ->
            {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
    end.

%% step is immediately followed after authentication
forbidden(Req, State) ->
    case cowboy_req:binding(box_path, Req) of
        undefined ->    % = no additional path given
            NewState = State#state{
                ibo_boxindices = box_server:get_boxindices(State#state.ibo_user),
                type = overview
            },
            {false, Req, NewState};
        _XboID ->
            {true, Req, State#state{type = detail}} % TODO: implement retrieving one instance of type ibo_boxdata
        % retrieve detail and check if the user has access to it
        % if not found -> send err404 and return {stop,Req,State}
        % if no access -> return false, otherwise true
%%            case valid_path(PasteID) and file_exists(PasteID) of
%%                true -> {true, Req, PasteID};
%%                false -> {false, Req, PasteID}
%%            end
    end.

%% ------------------------

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

to_json(Req, State) when State#state.type =:= overview ->
    PrepareData = [ json_helper:prepare(E) || E <-State#state.ibo_boxindices],
    Body = jsx:encode(PrepareData),
    {Body, Req, State}.