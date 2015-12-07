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
    ibo_boxdetail :: any() | undefined,
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
        XboID ->
            io:format("XboID>>>~p~n", [XboID]),
            case box_server:get_webinit(XboID) of
                {error, not_found} ->
                    {ok, Req2} = cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"id not found\"}">>, Req),
                    {stop, Req2, State};
                {error, _Reason} ->
                    {ok, Req2} = cowboy_req:reply(500, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"db problem\"}">>, Req),
                    {stop, Req2, State};
                Data ->
                    io:format("Data>>>~p~n", [Data]),
                    {Group, Args} = Data,
                    case lists:member(Group, State#state.ibo_user#ibo_user.groups) of
                        true ->
                            {false, Req, State#state{type = detail, ibo_boxdetail = Args}};
                        false ->
                            {true, Req, State#state{type = detail, ibo_boxdetail = Args}}
                    end
            end
    end.

%% ------------------------

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json}
    ], Req, State}.

to_json(Req, State) when State#state.type =:= overview ->
    PrepareData = [json_helper:prepare(E) || E <- State#state.ibo_boxindices],
    Body = jsx:encode(PrepareData),
    {Body, Req, State};
to_json(Req, State) when State#state.type =:= detail ->
    PrepareData = State#state.ibo_boxdetail,
    Body = jsx:encode(PrepareData),
    {Body, Req, State}.
