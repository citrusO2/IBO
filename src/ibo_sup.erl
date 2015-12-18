%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Nov 2015 22:47
%%%-------------------------------------------------------------------
-module(ibo_sup).
-author("Florian").

%% supervisor --------------------------------------------------------
-behaviour(supervisor).
-export([init/1]).

%% API ---------------------------------------------------------------
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Helper macro for declaring children of supervisor -----------------
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(directory_server, worker),
        ?CHILD(box_server, worker),
        ?CHILD(xbo_router, worker, [[["box_server","blub_server","another_server"]]]) % Args = Allowed Services
    ]} }.

%%%===================================================================
%%% Internal functions
%%%===================================================================