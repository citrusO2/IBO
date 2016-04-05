%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2016 18:17
%%%-------------------------------------------------------------------
-module(iactor_sup).
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
    SupFlags = ?FLAGS(one_for_one, 5, 10),
    {ok, {SupFlags, []}}.   % no children added at the beginning
