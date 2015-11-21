%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     Server where IBOs are stored for access via web
%%% @end
%%% Created : 21. Nov 2015 17:41
%%%-------------------------------------------------------------------
-module(box_server).
-author("Florian").

-include("box_records.hrl").

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/0, stop/0, handle_ibo/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

handle_ibo(IBO, Step) ->    % main function where IBOs get send to from other servers
    gen_server:call(?MODULE, {handle_ibo, IBO, Step}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}. % 0 = initial state

handle_call({handle_ibo, IBO, Step}, _From, N) ->
    {reply, store_ibo(IBO, Step), N + 1};
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
store_ibo(IBO, Step) ->         % TODO consider correlation ID to "merge" several IDs (correlation ID should be saved in step) -> correlation ID has to be set when creating IBO
    Id = "IBO",                 % TODO retrieve IBO-ID from IBO
    GroupName = "Testgroup",    % TODO retrieve Groupname from Step itself
    BoxRec = #ibo_boxdata{iboid = Id, ibodata = IBO,ibostep = Step},
    Res = mnesia:transaction(
        fun() ->
            mnesia:write(BoxRec),
            case mnesia:wread({ibo_boxindex, GroupName}) of
                [R] ->
                    mnesia:write(R#ibo_boxindex{
                        ibolist = [Id|R#ibo_boxindex.ibolist]
                    });
                [] ->
                    mnesia:write(#ibo_boxindex{groupname = GroupName, ibolist = [id]})
            end
        end),
    case Res of
        {atomic, ok} -> ok;
        _ -> {error, "Write failure"}
    end.