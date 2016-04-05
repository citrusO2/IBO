%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2016 01:07
%%%-------------------------------------------------------------------
-module(watchdog_sup).
-author("Florian").

-include("../helper/supervisor_macros.hrl").

%% supervisor
-behaviour(supervisor).
-export([init/1]).

%% API
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init([]) ->
    SupFlags = ?FLAGS(one_for_all, 5, 10),
    {ok, {SupFlags, [
        ?CHILD(iactor_sup, supervisor),
        ?CHILD(watchdog_server, worker)
    ]}}.   % no children added at the beginning
