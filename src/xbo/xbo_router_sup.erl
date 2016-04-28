%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2016 16:13
%%%-------------------------------------------------------------------
-module(xbo_router_sup).
-author("Florian").

-include("../helper/supervisor_macros.hrl").

%% supervisor
-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/1]).

start_link(Args) ->
    supervisor:start_link(?MODULE, Args).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init(Args) ->
    SupFlags = ?FLAGS(one_for_all, 5, 10),
    WebServerChildSpec = ?CHILD(xbo_router,worker, [Args]),
    {ok, {SupFlags, [WebServerChildSpec]}}.
