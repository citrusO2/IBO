%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Mai 2016 22:19
%%%-------------------------------------------------------------------
-module(box_sup).
-author("Florian").

-include("../helper/supervisor_macros.hrl").

%% supervisor
-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/1, xlib_info/0]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

xlib_info() ->
    box_server:xlib_info().

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init(Args) ->
    SupFlags = ?FLAGS(one_for_all, 5, 10),
    RepoServerChildSpec = ?CHILD(box_server,worker, [Args]),
    {ok, {SupFlags, [RepoServerChildSpec]}}.