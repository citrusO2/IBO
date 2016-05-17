%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     watchdog server api wrapped in remote calls
%%% @end
%%% Created : 09. Mai 2016 16:08
%%%-------------------------------------------------------------------
-module(watchdog_remote).
-author("Florian").

%% API
-export([start_iactor/3, stop_iactor/2, get_iactors/1, get_xactors/1, connect/2, disconnect/2, connection_status/1]).

start_iactor(HostNode, IactorType, Args) ->
    rpc:call(HostNode, watchdog_server, start_iactor, [IactorType, Args]).

stop_iactor(HostNode, Name) ->
    rpc:call(HostNode, watchdog_server, stop_iactor, [Name]).

get_iactors(HostNode) ->
    rpc:call(HostNode, watchdog_server, get_iactors, []).

get_xactors(HostNode) ->
    rpc:call(HostNode, watchdog_server, get_xactors, []).

connect(HostNode, Node) ->
    rpc:call(HostNode, watchdog_server, connect, [Node]).

disconnect(HostNode, Node) ->
    rpc:call(HostNode, watchdog_server, disconnect, [Node]).

connection_status(HostNode) ->
    rpc:call(HostNode, watchdog_server, connection_status, []).