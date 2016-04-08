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
-export([start_link/1]).

start_link(Args) ->
    %supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    supervisor:start_link(?MODULE, Args).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================
init(Args) ->
    SupFlags = ?FLAGS(one_for_all, 5, 10),
    RepoServerChildSpec = ?CHILD(repo_server,worker, [Args]),
    {ok, {SupFlags, [RepoServerChildSpec]}}.
