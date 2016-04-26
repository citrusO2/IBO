%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2016 22:35
%%%-------------------------------------------------------------------
-module(deadletter_sendworker_sup).
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
    SupFlags = ?FLAGS(simple_one_for_one, 5, 10),
    SendworkerChildSpec = #{
        id => deadletter_sendworker,
        start => {deadletter_sendworker, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [deadletter_sendworker]},
    {ok, {SupFlags, [SendworkerChildSpec]}}.
