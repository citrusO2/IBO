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
-behaviour(xbo_endpoint_behaviour).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/0, stop/0, process_xbo/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

process_xbo(XBO, StepNr) ->    % main function where IBOs get send to from other servers
    gen_server:call(?MODULE, {process_xbo, XBO, StepNr}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, #ibo_boxserver_state{domain = atom_to_list(?MODULE)}}. % initial state

handle_call({process_xbo, XBO, StepNr}, _From, State) ->
    try check_xbo(XBO, StepNr, State) of
        ok ->
            {reply, store_xbo(XBO, StepNr), State}
    catch
        _:Error ->
            {reply, {error,{check_xbo,Error}}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

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
    NewBoxIndElemPrev = #ibo_boxindex_elementpreview{
        xboid = XBOid,
        xbostepdescription = StepDescription,
        xbotemplate = XBOtemplate,
        storedate = os:timestamp()},
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

check_xbo(XBO, StepNr, State) ->
    StepCount = length(XBO#ibo_xbo.steps),
    throw_if_true(StepNr > StepCount, "StepNr outside StepRange"),

    Step = lists:nth(StepNr, XBO#ibo_xbo.steps),
    throw_if_false(Step#ibo_xbostep.domain =:= State#ibo_boxserver_state.domain, "Step is for a different domain"),

    XBOid = XBO#ibo_xbo.id,
    throw_if_true(XBOid =:= "", "Id of XBO must not be empty"),

    % TODO check commands as well
    % check if XBO already exists in database (=duplicate XBO) as last check -> slowest check
    throw_if_true(is_key_in_table(ibo_boxdata,XBOid), "XBO is already in Table"),

    ok.
%%%===================================================================
%%% Helper functions
%%%===================================================================
throw_if_false(Expression,ThrowReason) ->
    case Expression of
        true ->
            ok;
        _ ->
            throw(ThrowReason)
    end.
throw_if_true(Expression,ThrowReason)->
    throw_if_false(not Expression,ThrowReason).

is_key_in_table(Table, Key) ->
    Res = mnesia:transaction(
        fun() ->
            mnesia:read(Table, Key)
        end),
    case Res of
        {atomic, [_]} -> true;
        {atomic, []} -> false;
        _ -> throw("Cannot check if Key is in Table")
    end.