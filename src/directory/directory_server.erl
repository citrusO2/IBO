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

%% directory_server internal state -----------------------------------------
-record(state, {
    name :: binary()
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/1, stop/1,
    get_user/2, search_user/2,
    get_group/2, write_group/2, search_group/2, get_all_groups/1,
    create_user/3, update_user/3, get_user_info/3, resolve_usergroups/2]).

%% starts a new global directory server with the given name as the global name
-spec start_link(Args :: #{name => binary()}) ->  {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(Args) ->
    Name = maps:get(name, Args),
    gen_server:start_link({global, Name}, ?MODULE, Args, []).

%% stops a directory by its name
-spec stop(Directory :: binary()) -> ok.
stop(Directory) ->
    gen_server:call({global, Directory}, stop).

%% retrieves a user by the username
-spec get_user(Directory :: binary(), Username :: binary()) -> #ibo_user{} | not_found | {error, term()}.
get_user(Directory, Username) ->
    gen_server:call({global, Directory}, {get_user, Username}).

%% searches users by lastname, depending on the searchstring, empty string = all items
-spec search_user(Directory :: binary(), SearchString :: binary()) -> [#ibo_user{}] | [] | {error, term()}.
search_user(Directory, SearchString) ->
    gen_server:call({global, Directory}, {search_user, SearchString}).

%% retrieves a group by its name
-spec get_group(Directory :: binary(), Groupname :: binary()) -> #ibo_group{} | not_found | {error, term()}.
get_group(Directory, Groupname) ->
    gen_server:call({global, Directory}, {get_group, Groupname}).

%% writes a group to the db, if group already exists, it gets overwritten
-spec write_group(Directory :: binary(), Group :: #ibo_group{}) -> ok | {error, term()}.
write_group(Directory, Group) ->
    gen_server:call({global, Directory}, {write_group, Group}).

%% same as search_user, just for groups by groupname
-spec search_group(Directory :: binary(), SearchString :: binary()) -> [#ibo_group{}] | [] | {error, term()}.
search_group(Directory, SearchString) ->
    gen_server:call({global, Directory}, {search_group, SearchString}).

%% syntactic sugar to retrieve all groups via search_group
-spec get_all_groups(Directory :: binary()) -> [#ibo_group{}] | [] | {error, term()}.
get_all_groups(Directory) ->
    search_group(Directory, <<"">>).

%%-------------------------------------
% Higher API functions -- TODO: remove lower functions with higher ones
%%-------------------------------------

%% creates a new user on the given directory and stores the password encrypted. Only creates a user if the username is not yet taken, otherwise there will be a password error or if the password is right, the user gets overwritten
-spec create_user(Directory :: binary(), User :: #ibo_user{}, Password :: binary()) ->  ok | {error, term()}.
create_user(Directory, User, Password) ->
    gen_server:call({global, Directory}, {create_user, User, Password}).

%% exactly like create user
update_user(Directory, User, Password) ->
    create_user(Directory, User, Password).

%% retrieves a user by username and password, gives back an error when password is wrong or the username cannot be found. The Password gets removed from the result record
-spec get_user_info(Directory :: binary(), Username :: #ibo_user{}, Password :: binary()) -> #ibo_user{} | {error, term()}.
get_user_info(Directory, Username, Password) ->
    gen_server:call({global, Directory}, {get_user_info, Username, Password}).

%% retrieves all groups of a user recursively
-spec resolve_usergroups(Directory :: binary(), Groups :: [binary()]) -> [binary()] | not_found | {error, term()};
                        (Directory :: binary(), User :: #ibo_user{}) -> [binary()] | not_found | {error, term()};
                        (Directory :: binary(), Username :: binary()) -> [binary()] | not_found | {error, term()}.
resolve_usergroups(Directory, Groups) when is_list(Groups)->
    gen_server:call({global, Directory}, {resolve_usergroups, Groups});
resolve_usergroups(Directory, User) when is_record(User, ibo_user)->
    resolve_usergroups(Directory, User#ibo_user.groups);
resolve_usergroups(Directory, Username) when is_binary(Username) ->
    case get_user(Directory, Username) of
        User when is_record(User, ibo_user) ->
            resolve_usergroups(Directory, User);
        Else -> Else    % propagate error from get_user
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    Name = maps:get(name, Args),
    io:format("~p (~p) starting~n", [?MODULE, Name]),
    create_tables_if_nonexistent(),
    {ok, #state{name = Name}}. % 0 = initial state

handle_call({get_user, Username}, _From, S) ->
    {reply, db:read_transactional(ibo_user, Username), S};
handle_call({search_user, SearchString}, _From, S) ->
    {reply, search_transactional(SearchString, ibo_user, 4), S}; % 4 = lastname
handle_call({get_group, Groupname}, _From, S) ->
    {reply, db:read_transactional(ibo_group, Groupname), S};
handle_call({write_group, Group}, _From, S) ->
    {reply, db:write_transactional(Group), S};
handle_call({search_group, SearchString}, _From, S) ->
    {reply, search_transactional(SearchString, ibo_group, 2), S}; % 2 = groupname
handle_call({create_user, User, Password}, _From, S) ->
    {reply, create_or_update_user(User, Password), S};
handle_call({get_user_info, Username, Password}, _From, S) ->
    {reply, read_user_info(Username, Password), S};
handle_call({resolve_usergroups, Groups}, _From, S) ->
    {reply, resolve_groups_transactional(Groups), S};
handle_call(stop, _From, S) ->
    {stop, normal, stopped, S}.

handle_cast(_Msg, S) -> {noreply, S}.
handle_info(_Info, S) -> {noreply, S}.
terminate(_Reason, _S) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%do(qlc:q([X || X <- mnesia:table(ibo_directory), X#user.username =:= Username])).

create_tables_if_nonexistent() ->
    db:create_local_table_if_nonexistent(ibo_user,
        record_info(fields, ibo_user),
        disc_copies, set),
    db:create_local_table_if_nonexistent(ibo_group,
        record_info(fields, ibo_group),
        disc_copies, set),
    ok = mnesia:wait_for_tables([ibo_user, ibo_group], 5000).

resolve_groups_transactional(StartGroups) ->  % start function of recursion
    Res = mnesia:transaction(
        fun() ->
            resolve_groups_rec(StartGroups, sets:new())    % resolve groups, using the already given groups as the starting point
        end),
    case Res of
        {aborted, Reason} -> {error, Reason};
        {atomic, Groups} -> Groups
    end.

resolve_groups_rec([], Accu) -> sets:to_list(Accu);   % no more groups to resolve

resolve_groups_rec([CurrentGroup | OtherGroups], Accu) ->
    case sets:is_element(CurrentGroup, Accu) of   % when group is in Accu => current group was already resolved, to avoid endless loops
        true ->
            resolve_groups_rec(OtherGroups, Accu);
        false ->
            NewAccu = sets:add_element(CurrentGroup, Accu),
            case mnesia:read(ibo_group, CurrentGroup) of
                [Group] ->
                    ParentGroups = Group#ibo_group.parents,
                    NewGroupsToResolve = lists:append(OtherGroups,ParentGroups),    % consider using set instead of list
                    resolve_groups_rec(NewGroupsToResolve, NewAccu);
                [] ->
                    resolve_groups_rec(OtherGroups, NewAccu)    % parent group couldn't be found (= dead group)
            end
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
is_substring_in_string(Source, Find) when is_binary(Source), is_binary(Find)->
    case binary:match(Source, Find) of
        nomatch -> false;
        _Matched -> true
    end;
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