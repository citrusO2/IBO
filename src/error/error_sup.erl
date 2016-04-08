%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2016 21:27
%%%-------------------------------------------------------------------
-module(error_sup).
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
    ErrorServerChildSpec = ?CHILD(error_server,worker, [Args]),
    {ok, {SupFlags, [ErrorServerChildSpec]}}.
