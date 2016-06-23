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

-include("../helper/type_specs.hrl").
-include("../directory/directory_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% web_server internal state ----------------------------------------
-record(state, {
    servers :: #{directory => binary(), box => binary(), repo => binary()},   % global name for the respective servers
    name :: binary()
}).
-record(ibo_webserver_user_cache, {
    session_id :: binary(),
    user :: #ibo_user{},
    timestamp :: timestamp()
}).
-record(ibo_webserver_username_cache, { % only one session can be stored per user -> second login invalidates other session
    username :: binary(),
    session_id :: binary()
}).
-define(CACHE_TIME_SECS, 28800). % = 8h
-define(CACHE_TIME_MSECS, ?CACHE_TIME_SECS * 1000).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/1, stop/1, get_cached_user/2, cache_user/2, clear_user_cache/3]).

start_link(Args) when is_map(Args) ->
    Name = maps:get(name, Args),
    gen_server:start_link({global, Name}, ?MODULE, Args, []).

stop(Webserver) ->
    gen_server:call({global, Webserver}, stop).

% function for getting cached users (session management)
-spec get_cached_user(Webserver :: binary(), SessionId :: binary()) -> #ibo_user{} | not_found | {error, term()}.
get_cached_user(Webserver, SessionId) ->
    gen_server:call({global, Webserver}, {get_cached_user, SessionId}).

% function for caching users (session management)
-spec cache_user(Webserver :: binary(), User :: #ibo_user{}) -> binary() | {error, term()}.
cache_user(Webserver, User) ->
    gen_server:call({global, Webserver}, {cache_user, User}).

-spec clear_user_cache(Webserver :: binary(), SessionId :: binary(), UserName :: binary()) -> ok | {error, term()}.
clear_user_cache(Webserver, SessionId, UserName) ->
    gen_server:call({global, Webserver}, {clear_user_cache, SessionId, UserName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(#{directory := _Directory, box := _Box, repo := _Repo, name := Name} = Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    Name = maps:get(name, Args),
    io:format("~p (~p) starting~n", [?MODULE, Name]),
    create_tables_if_nonexistent(),
%%    {ok, _} = timer:send_interval(1000 * 60, tick), % every minute

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {file, "./src/webclient/index.html"}},
            {"/api/box/[:box_path]", box_handler, Args},
            {"/api/directory/[:dir_type]", directory_handler, Args},
            {"/api/directory/[:dir_type]/[:user_path]", directory_handler, Args},
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
handle_call( {get_cached_user, SessionId}, _From, State) ->
    {reply, retrieve_and_update_user(SessionId), State};
handle_call( {cache_user, User}, _From, State) ->
    {reply, secure_cache_user(User), State};
handle_call( {clear_user_cache, SessionId, UserName}, _From, State) ->
    {reply, delete_session_and_username_cache(SessionId, UserName) , State};
handle_call(stop, _From, State) ->
    cowboy:stop_listener(http),
    {stop, normal, stopped, State}.
handle_cast(_Msg, State) -> {noreply, State}.
%%handle_info(tick, State) ->
%%    clear_old_cache(),    % clearing old cached users
%%    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    cowboy:stop_listener(http),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_tables_if_nonexistent() ->
    db:create_local_table_if_nonexistent(ibo_webserver_user_cache,
        record_info(fields, ibo_webserver_user_cache),
        ram_copies, set),
    db:create_local_table_if_nonexistent(ibo_webserver_username_cache,
        record_info(fields, ibo_webserver_username_cache),
        ram_copies, set),
    ok = mnesia:wait_for_tables([ibo_webserver_user_cache, ibo_webserver_username_cache], 5000).

% cache users but disallow having the same user logged in multiple times!
secure_cache_user(User) ->
    Id = create_session_id(),
    Res = mnesia:transaction(
        fun() ->
            mnesia:write(#ibo_webserver_user_cache{session_id = Id, timestamp = os:timestamp(), user = User}),
            case mnesia:wread({ibo_webserver_username_cache, User#ibo_user.username}) of
                [UsernameCache] ->  % user is already logged in!
                    mnesia:delete({ibo_webserver_user_cache, UsernameCache#ibo_webserver_username_cache.session_id}); % delete other session
                [] -> ok% user is not logged in yet
            end,
            mnesia:write(#ibo_webserver_username_cache{session_id = Id, username = User#ibo_user.username})
        end),
    case Res of
        {atomic, ok} -> Id;
        _ -> {error, "Write failure"}
    end.

% try to retrieve user by session id, if found -> update timestamp
retrieve_and_update_user(SessionId) ->
    Res = mnesia:transaction(
        fun() ->
            case mnesia:wread({ibo_webserver_user_cache, SessionId}) of
                [UserCache] ->
                    mnesia:write(UserCache#ibo_webserver_user_cache{timestamp = os:timestamp()}),
                    [UserCache#ibo_webserver_user_cache.user];
                [] -> []
            end
        end),
    case Res of
        {atomic, [User]} -> User;
        {atomic, []} -> not_found;
        _ -> {error, "Read failure"}
    end.

% delete the cache for e.g. logout
delete_session_and_username_cache(SessionId, UserName) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:delete({ibo_webserver_user_cache, SessionId}),
            mnesia:delete({ibo_webserver_username_cache, UserName})
            end),
    case Res of
        {atomic, ok} -> ok;
        _ -> {error, "Delete failure"}
    end.

%%clear_old_cache() ->
%%    Now = os:timestamp(),
%%    mnesia:transaction(
%%        fun() ->
%%            Q = qlc:q([R#ibo_webserver_user_cache.session_id || R <- mnesia:table(ibo_webserver_user_cache),
%%                  timer:now_diff(Now, R#ibo_webserver_user_cache.timestamp) >= ?CACHE_TIME_MSECS ]),
%%            List = qlc:e(Q),
%%            lists:foreach(fun(Key) -> mnesia:delete({ibo_webserver_user_cache, Key}) end, List)
%%        end).

%%%===================================================================
%%% Helper
%%%===================================================================
create_session_id() ->
    base64:encode(crypto:strong_rand_bytes(80)).   % cookie must be string