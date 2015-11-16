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
-export([install/1]).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(ibo_user,
        [{attributes, record_info(fields, ibo_user)},
            {disc_copies, Nodes},
            {type, set}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    mnesia:wait_for_tables([ibo_user],5000),
    ibo_sup:start_link().

stop(_State) ->
    ok.
