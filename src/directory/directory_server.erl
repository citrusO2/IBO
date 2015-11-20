%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2015 00:24
%%%-------------------------------------------------------------------
-module(directory_server).
-author("Florian").

-include("directory_records.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% specs -------------------------------------------------------------
-spec get_user(nonempty_string()) -> #ibo_user{}.
-spec write_user(#ibo_user{}) -> ok | any().

%% API ---------------------------------------------------------------
-export([start_link/0, stop/0,
    get_user/1, write_user/1, search_user/1,
    get_group/1, write_group/1, search_group/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_user(Username) ->
    gen_server:call(?MODULE, {get_user, Username}).

write_user(User) ->
    gen_server:call(?MODULE, {write_user, User}).

search_user(SearchString) ->
    gen_server:call(?MODULE, {search_user, SearchString}).

get_group(Groupname) ->
    gen_server:call(?MODULE, {get_group, Groupname}).

write_group(Group) ->
    gen_server:call(?MODULE, {write_group, Group}).

search_group(SearchString) ->
    gen_server:call(?MODULE, {search_group, SearchString}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}. % 0 = initial state

handle_call({get_user, Username}, _From, N) ->
    {reply, read_transactional(ibo_user, Username), N + 1};
handle_call({write_user, User}, _From, N) ->
    {reply, write_transactional(User), N + 1};
handle_call({search_user, SearchString}, _From, N) ->
    {reply, search_transactional(SearchString, ibo_user, 4), N + 1}; % 4 = lastname
handle_call({get_group, Groupname}, _From, N) ->
    {reply, read_transactional(ibo_group, Groupname), N + 1};
handle_call({write_group, Group}, _From, N) ->
    {reply, write_transactional(Group), N + 1};
handle_call({search_group, SearchString}, _From, N) ->
    {reply, search_transactional(SearchString, ibo_group, 2), N + 1}; % 2 = groupname
handle_call(stop, _From, N) ->
    {stop, normal, stopped, N}.

handle_cast(_Msg, N) -> {noreply, N}.
handle_info(_Info, N) -> {noreply, N}.
terminate(_Reason, _N) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, N, _Extra) -> {ok, N}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%do(qlc:q([X || X <- mnesia:table(ibo_directory), X#user.username =:= Username])).

read_transactional(Table, Key) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:read(Table, Key)
        end),
    case Res of
        {atomic, [Record]} -> Record;
        {atomic, []} -> not_found;
        _ -> {error, "Read failure"}
    end.

write_transactional(Record) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:write(Record)
        end),
    case Res of
        {atomic, ok} -> ok;
        _ -> {error, "Write failure"}
    end.

search_transactional(SearchString,Table,_ElementPosition) when SearchString =:= "" ->
    Res = mnesia:transaction(
        fun() ->
            qlc:e(mnesia:table(Table))
        end),
    case Res of
        {atomic, X} when is_list(X) -> X;
        _ -> {error, "Search failure"}
    end;
search_transactional(SearchString,Table,ElementPosition) ->
    Res = mnesia:transaction(
        fun() ->
            Q = qlc:q([R || R <- mnesia:table(Table),
                is_substring_in_string(element(ElementPosition,R), SearchString)]),
            qlc:e(Q)
        end),
    case Res of
        {atomic, X} when is_list(X) -> X;
        _ -> {error, "Search failure"}
    end.

%%%===================================================================
%%% Helper functions
%%%===================================================================
-spec is_substring_in_string(nonempty_string(), nonempty_string()) -> boolean().
is_substring_in_string(Source, Find) ->
    string:str(Source, Find) >= 1. % true if found, false if not found