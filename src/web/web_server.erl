%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Apr 2016 16:01
%%%-------------------------------------------------------------------
-module(web_server).
-author("Florian").

%% web_server internal state ----------------------------------------
-record(state, {
    servers :: #{directory => binary(), box => binary(), repo => binary()},   % global name for the respective servers
    name :: binary()
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/1, stop/1]).

start_link(Args) when is_map(Args) ->
    Name = maps:get(name, Args),
    gen_server:start_link({global, Name}, ?MODULE, Args, []).

stop(Webserver) ->
    gen_server:call({global, Webserver}, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(#{directory := _Directory, box := _Box, repo := _Repo, name := Name} = Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, "./src/webclient/index.html"}},
            {"/api/box/[:box_path]", box_handler, Args},
            {"/api/directory/[:user_path]", directory_handler, Args},
            {"/api/repo/[:repo_type]", repo_handler, Args},
            {"/api/repo/[:repo_type]/[:repo_path]", repo_handler, Args},
            {"/[...]", cowboy_static, {dir, "./src/webclient", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),

    {ok, #state{servers = Args, name = Name}}. % initial state

handle_call( testcall, _From, State) ->
    {reply, ok, State};
handle_call(stop, _From, State) ->
    cowboy:stop_listener(http),
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    cowboy:stop_listener(http),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.