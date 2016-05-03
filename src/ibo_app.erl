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
-export([install/0, install/1, uninstall/1, start_dependencies/0, stop_dependencies/0, installtestdata/0]).

install() ->
    mnesia:stop(),
    install([node()]),
    mnesia:start().

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
    mnesia:create_table(ibo_boxdata,
        [{attributes, record_info(fields, ibo_boxdata)},
            {disc_copies, Nodes},
            {type, set}]),
    mnesia:create_table(ibo_boxindex,
        [{attributes, record_info(fields, ibo_boxindex)},
            {disc_copies, Nodes},
            {type, set}]),
    mnesia:create_table(ibo_repo_template,
        [{attributes, record_info(fields, ibo_repo_template)},
            {disc_copies, Nodes},
            {type, set}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).


uninstall(Nodes) ->
    mnesia:delete_table(ibo_user),
    mnesia:delete_table(ibo_group),
    mnesia:delete_table(ibo_boxdata),
    mnesia:delete_table(ibo_boxindex),
    mnesia:delete_table(ibo_repo_template),
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

installtestdata()->
    repo_exampletemplates:install(),
    directory_exampleusers:install(),
    ok.

%%start_web() ->
%%    io:format("cowboy webserver starting~n"),
%%    Dispatch = cowboy_router:compile([
%%        {'_', [
%%            {"/", cowboy_static, {file, "./src/webclient/index.html"}},
%%            {"/api/box/[:box_path]", box_handler, []},
%%            {"/api/directory/[:user_path]", directory_handler, []},
%%            {"/api/repo/[:repo_path]", repo_handler, []},
%%            {"/[...]", cowboy_static, {dir, "./src/webclient", [{mimetypes, cow_mimetypes, all}]}}
%%        ]}
%%    ]),
%%    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
%%        [{env, [{dispatch, Dispatch}]}]
%%    ).

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
