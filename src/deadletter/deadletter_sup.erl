%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2016 22:33
%%%-------------------------------------------------------------------
-module(deadletter_sup).
-author("Florian").

-include("../helper/supervisor_macros.hrl").

%% supervisor
-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/1]).

start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init(Args) ->
    SupFlags = ?FLAGS(one_for_all, 5, 10),
    {ok, {SupFlags, [
        ?CHILD(deadletter_sendworker_sup, supervisor),
        ?CHILD(deadletter_server, worker, [Args])
    ]}}.
