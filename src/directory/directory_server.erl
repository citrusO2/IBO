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
% TODO write more specs for API

%% API ---------------------------------------------------------------
-export([start_link/0, stop/0,
    get_user/1, write_user/1, search_user/1,
    get_group/1, write_group/1, search_group/1,
    create_user/2, update_user/2, get_user_info/2]).

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

% Higher API functions -- TODO: remove lower functions with higher ones
create_user(User, Password) ->
    gen_server:call(?MODULE, {create_user, User, Password}).

update_user(User, Password) ->
    create_user(User, Password).

get_user_info(Username, Password) ->
    gen_server:call(?MODULE, {get_user_info, Username, Password}).

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
handle_call({create_user, User, Password}, _From, N) ->
    {reply, create_or_update_user(User, Password), N + 1};
handle_call({get_user_info, Username, Password}, _From, N) ->
    {reply, read_user_info(Username, Password), N + 1};
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

search_transactional(SearchString, Table, _ElementPosition) when SearchString =:= "" orelse SearchString =:= <<"">> ->
    Res = mnesia:transaction(
        fun() ->
            qlc:e(mnesia:table(Table))
        end),
    case Res of
        {atomic, L} when is_list(L) -> override_password(L);
        _ -> {error, "Search failure"}
    end;
search_transactional(SearchString, Table, ElementPosition) ->
    Res = mnesia:transaction(
        fun() ->
            Q = qlc:q([R || R <- mnesia:table(Table),
                is_substring_in_string(element(ElementPosition, R), SearchString)]), % TODO check if mnesia's match_object function or select function is better suited
            qlc:e(Q)
        end),
    case Res of
        {atomic, L} when is_list(L) -> override_password(L);
        _ -> {error, "Search failure"}
    end.

create_or_update_user(User, Password) ->
    Username = User#ibo_user.username,
    Res = mnesia:transaction(
        fun() ->
            case mnesia:wread({ibo_user, Username}) of
                [StoredUser] ->
                    case is_password_correct(StoredUser, Password) of
                        true ->
                            mnesia:write(User#ibo_user{password = create_protected_password(Password)});
                        false ->
                            mnesia:abort("Incorrect password")
                    end;
                [] ->
                    NewUser = User#ibo_user{password = create_protected_password(Password)},
                    mnesia:write(NewUser)
            end
        end),
    case Res of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason};
        _ -> {error, "Write failure"}
    end.

read_user_info(Username, Password) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:read(ibo_user, Username)
        end),
    case Res of
        {atomic, [User]} ->
            case is_password_correct(User, Password) of
                true ->
                    override_password(User);
                false ->
                    {error, "Wrong password"}
            end;
        {atomic, []} -> {error, "User not found"};
        _ -> {error, "Read failure"}
    end.

%%%===================================================================
%%% Helper functions
%%%===================================================================
-spec is_substring_in_string(nonempty_string(), nonempty_string()) -> boolean().
is_substring_in_string(Source, Find) ->
    string:str(Source, Find) >= 1. % true if found, false if not found

% TODO: make directory_server parallel, protected password creation = compute expensive
create_protected_password(ClearPassword) ->
    Salt = crypto:strong_rand_bytes(64),
    Iterations = 20000,
    DerivedLength = 64,
    {ok, Key} = pbkdf2:pbkdf2(sha512, ClearPassword, Salt, Iterations, DerivedLength),
    {Salt, Key}.

is_password_correct(User, ClearPassword) ->
    Salt = element(1, User#ibo_user.password),
    StoredKey = element(2, User#ibo_user.password),
    Iterations = 20000,
    DerivedLength = 64,
    {ok, NewKey} = pbkdf2:pbkdf2(sha512, ClearPassword, Salt, Iterations, DerivedLength),

    NewKey =:= StoredKey.

% overrides password but keeps the format intact, otherwise the record gets invalid
override_password(List) when is_list(List) ->
    [override_password(E) || E <- List];
override_password(User) when is_record(User, ibo_user) ->
    User#ibo_user{password = undefined};
override_password(Else) when is_tuple(Else) ->  % do nothing if record isn't of type ibo_user
    Else.