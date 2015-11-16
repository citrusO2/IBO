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

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% specs -------------------------------------------------------------
-spec get_user(nonempty_string()) -> #ibo_user{}.

%% API ---------------------------------------------------------------
-export([start_link/0, stop/0, get_user/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_user(Username) ->
    gen_server:call(?MODULE, {get_user, Username}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    %% Note we must set trap_exit = true if we
    %% want terminate/2 to be called when the application
    %% is stopped
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({get_user, Username}, _From, N) ->
    {reply, read_transactional(ibo_user, Username), N + 1};
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
        {atomic, [Record]}-> Record;
        _ -> Res
    end.