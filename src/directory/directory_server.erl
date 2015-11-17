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
-export([start_link/0, stop/0, get_user/1, write_user/1, search_user/1]).

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
handle_call(stop, _From, N) ->
    {stop, normal, stopped, N};
handle_call({search_user, SearchString}, _From, N) ->
    {reply, search_user_transactional(SearchString), N + 1}.

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

search_user_transactional(SearchString) ->
    Res = mnesia:transaction(fun() ->
        Q = qlc:q([U || U <- mnesia:table(ibo_user),
            is_substring_in_string(U#ibo_user.lastname, SearchString)]),
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
    Pos = string:str(Source, Find),
    io:format("Out: [~p][~p]~n", [Source, Find]),
    if
        Pos >= 1 ->
            io:format("True"),
            true;
        true ->
            io:format("False"),
            false
    end.