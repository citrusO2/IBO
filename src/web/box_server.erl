%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     Server where XBOs are stored for access via web
%%% @end
%%% Created : 21. Nov 2015 17:41
%%%-------------------------------------------------------------------
-module(box_server).
-author("Florian").

-include("box_records.hrl").
-include("../xbo/xbo_records.hrl").

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/0, stop/0, handle_xbo/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

handle_xbo(XBO, StepNr) ->    % main function where IBOs get send to from other servers
    gen_server:call(?MODULE, {handle_xbo, XBO, StepNr}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}. % 0 = initial state

handle_call({handle_xbo, XBO, StepNr}, _From, N) ->
    {reply, store_xbo(XBO, StepNr), N + 1};   % TODO check validity of XBO information (necessary XBO parts, XBO step itself and XBO stepdata)
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
store_xbo(XBO, StepNr) ->   % TODO consider correlation ID to "merge" several IDs (correlation ID should be saved in step) -> correlation ID has to be set when creating XBO
    Step = lists:nth(StepNr, XBO#ibo_xbo.steps),
    GroupName = Step#ibo_xbostep.local,
    StepDescription = Step#ibo_xbostep.description,
    XBOid = XBO#ibo_xbo.id,
    XBOtemplate = XBO#ibo_xbo.template,

    % create new elements for the box and save them
    NewBoxRec = #ibo_boxdata{xboid = XBOid, xbodata = XBO, xbostepnr = StepNr},
    NewBoxIndElemPrev = #ibo_boxindex_elementpreview{xboid = XBOid,xbostepdescription = StepDescription, xbotemplate = XBOtemplate},
    Res = mnesia:transaction(
        fun() ->
            mnesia:write(NewBoxRec),
            case mnesia:wread({ibo_boxindex, GroupName}) of
                [R] ->
                    mnesia:write(R#ibo_boxindex{
                        xbolist = [NewBoxIndElemPrev|R#ibo_boxindex.xbolist]
                    });
                [] ->
                    mnesia:write(#ibo_boxindex{groupname = GroupName, xbolist = [NewBoxIndElemPrev]})
            end
        end),
    case Res of
        {atomic, ok} -> ok;
        _ -> {error, "Write failure"}
    end.