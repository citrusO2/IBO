%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2016 20:48
%%%-------------------------------------------------------------------
-module(repo_sup).
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
    RepoServerChildSpec = ?CHILD(repo_server,worker, [{["xbo_router"], ["should_be_deadletter_server"],"repo1",1}]),
    {ok, {SupFlags, [RepoServerChildSpec]}}.
