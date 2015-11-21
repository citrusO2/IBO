%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2015 22:47
%%%-------------------------------------------------------------------
-module(ibo_app).
-author("Florian").

-include("directory/directory_records.hrl").

%% application -------------------------------------------------------
-behaviour(application).
-export([start/2, stop/1]).

%% API ---------------------------------------------------------------
-export([install/1, start_dependencies/0]).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(ibo_user,
        [{attributes, record_info(fields, ibo_user)},
            {disc_copies, Nodes},
            {type, set}]),
    mnesia:create_table(ibo_group,
        [{attributes, record_info(fields, ibo_group)},
            {disc_copies, Nodes},
            {type, set}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

start_dependencies() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy).

start_web() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", toppage_handler, []}]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    ok.
%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    mnesia:wait_for_tables([ibo_user],5000),
    start_web(),
    ibo_sup:start_link().

stop(_State) ->
    % TODO stop cowboy
    ok.
