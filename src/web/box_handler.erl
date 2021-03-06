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
    ibo_boxindices :: [#ibo_boxindex{}] | undefined,
    xbo_id :: undefined | nonempty_string(),
    box_server_name :: binary(),
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
-export([json_get/2]).
-export([json_post/2]).

init(Req, Opts) ->
    Box = maps:get(box, Opts),
    Directory = maps:get(directory, Opts),
    WebServer = maps:get(name, Opts),
    {cowboy_rest, Req, #state{box_server_name = Box, directory_server_name = Directory, web_server_name = WebServer}}.

%% Allowed Methods ---------------------------------------------------
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

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
%% step is immediately followed after authentication
forbidden(Req, State) ->
    case cowboy_req:binding(box_path, Req) of
        undefined ->    % = no additional path given

            NewState = State#state{
                ibo_boxindices = box_server:get_boxindices(State#state.box_server_name, State#state.ibo_user),
                type = overview
            },
            {false, Req, NewState};
        XboID ->
            NewState = State#state{xbo_id = XboID},
%%            io:format("XboID>>>~p~n", [XboID]),
            case box_server:get_webinit(State#state.box_server_name, XboID) of
                {error, not_found} ->
                    cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"id not found\"}">>, Req),
                    {stop, Req, NewState};
                {error, _Reason} ->
                    cowboy_req:reply(500, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"db problem\"}">>, Req),
                    {stop, Req, NewState};
                Data ->
%%                    io:format("Data>>>~p~n", [Data]),
                    {Group, Args} = Data,
                    case lists:member(Group, NewState#state.ibo_user#ibo_user.access_to) of
                        true ->
                            {false, Req, NewState#state{type = detail, ibo_boxdetail = Args}};
                        false ->
                            {true, Req, NewState#state{type = detail, ibo_boxdetail = Args}}
                    end
            end
    end.

%% ------------------------

json_get(Req, State) when State#state.type =:= overview ->
    PrepareData = [json_helper:prepare(E) || E <- State#state.ibo_boxindices],
    Body = jsx:encode(PrepareData),
    {Body, Req, State};
json_get(Req, State) when State#state.type =:= detail ->
    PrepareData = State#state.ibo_boxdetail,
    Body = jsx:encode(PrepareData),
    {Body, Req, State}.

json_post(Req, State) when State#state.type =:= detail ->
    {ok, Body, Req2} = cowboy_req:body(Req),

    Data = jsx:decode(Body, [return_maps]),
    Schema = State#state.ibo_boxdetail,

    case schema_validator:validate_data(Schema, Data) of
        {ok, ValidData} ->
            case box_server:execute_xbo(State#state.box_server_name, State#state.xbo_id, ValidData) of
                {ok, xbo_send} ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], <<"{\"success\": \"xbo send\"}">>, Req);
                {ok, xbo_end} ->
                    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], <<"{\"success\": \"xbo finished\"}">>, Req);
                {error, _Reason} ->
                    io:format(">>>Error executing xbo, reason: ~p~n", [_Reason]),
                    cowboy_req:reply(500, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"error executing xbo\"}">>, Req)
            end,
            {true, Req2, State};
        {error, {_Reason, _ProblemValue}} ->
            % TODO log reason here!
            cowboy_req:reply(422, [{<<"content-type">>, <<"application/json">>}], <<"{\"error\": \"serverside validation failed\"}">>, Req),
            {true, Req2, State}
    end.
