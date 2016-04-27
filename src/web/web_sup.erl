%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2016 17:50
%%%-------------------------------------------------------------------
-module(web_sup).
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
    WebServerChildSpec = ?CHILD(web_server,worker, [Args]),
    {ok, {SupFlags, [WebServerChildSpec]}}.
