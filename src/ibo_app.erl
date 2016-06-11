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
-include("box/box_records.hrl").
-include("repo/repo_records.hrl").
-include("deadletter/deadletter_records.hrl").

%% application -------------------------------------------------------
-behaviour(application).
-export([start/2, stop/1]).

%% API ---------------------------------------------------------------
-export([install/0, install/1, uninstall/1, start_dependencies/0, stop_dependencies/0, install_testdata/0]).

install() ->
    mnesia:stop(),
    install([node()]),
    mnesia:start().

install(Nodes) ->
    ok = mnesia:create_schema(Nodes).

uninstall(Nodes) ->
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok = mnesia:delete_schema(Nodes).

start_dependencies() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    %application:set_env(mnesia, dir, "./ebin/mnesia"), % consider using configuration file
    ok = application:start(mnesia).

stop_dependencies() ->
    application:stop(mnesia),
    application:stop(cowboy),
    application:stop(cowlib),
    application:stop(ranch),
    application:stop(crypto),
    ok.

install_testdata()->
    watchdog_exampleconfig:install(),
    directory_exampleusers:install(<<"DIRECTORY1">>),

    ok.

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    mnesia:wait_for_tables([ibo_user],5000),
    %start_web(),
    ibo_sup:start_link().

stop(_State) ->
%%    io:format("cowboy webserver stopping~n"),
%%    cowboy:stop_listener(http),
    ok.
