%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     The box stores XBOs which need further input from other users or machines, before they can be executed further
%%%
%%%     process_xbo checks the XBO and stores it for further execution in the box
%%%     get_boxindices retrieves overview information of the XBOs, depending on the provided user or group names which where used to store them
%%%
%%% execute_xbo then really tries to execute the XBO with the given DataMap
%%%
%%%
%%% @end
%%% Created : 21. Nov 2015 17:41
%%%-------------------------------------------------------------------
-module(box_server).
-author("Florian").

-include("../directory/directory_records.hrl").
-include("../xlib/xlib_state.hrl").
-include_lib("stdlib/include/qlc.hrl").
-behaviour(xbo_endpoint_behaviour).

%% box_server internal state ------------------------------------------
-record(state, {
    domain :: binary(),
    workers = [] :: list({pid(), #xlib_state{}})
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/1, stop/1, process_xbo/3, get_boxindices/2, get_webinit/2, execute_xbo/3, xbo_childprocess/4]).

%% starts a new global box server with the given name as the global name
-spec start_link(Args :: #{name => binary()} ) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(Args) when is_map(Args)->
    Name = maps:get(name, Args),
    gen_server:start_link({global, Name}, ?MODULE, Args, []).

%% stops a box by its name
-spec stop(Box :: binary()) -> ok.
stop(Box) ->
    gen_server:call({global, Box}, stop).

%% processes a XBO on the box, but it is not actually executed yet, but simply stored
-spec process_xbo(Box :: binary(), XBO :: #ibo_xbo{}, StepNr :: non_neg_integer()) -> ok | {error, nonempty_string()}.
process_xbo(Box, XBO, StepNr) ->
    gen_server:call({global, Box}, {process_xbo, XBO, StepNr}). % main function where IBOs get send to from other servers, the important part is the {process_xbo,..,..} message that the box receives

%% actually executes the XBO because the necessary Data are provided via the given DataMap
-spec execute_xbo(Box :: binary(), XBOid :: binary(), DataMap :: #{}) -> {ok, xbo_end} | {ok, xbo_send} | {error, atom() | nonempty_string()}.
execute_xbo(Box, XBOid, DataMap) ->  %% TODO: add which User/Machine really executed the XBO
    gen_server:call({global, Box}, {execute_xbo, XBOid, DataMap}).

%% retrieves an overview for the given groupnames or the given user
-spec get_boxindices(Box :: binary, Groupnamelist :: nonempty_list(binary()) ) -> [] | [#ibo_boxindex{}] | {error, {nonempty_string(), MnesiaError :: term()}};
                    (Box :: binary, User :: #ibo_user{} ) -> [] | [#ibo_boxindex{}] | {error, {nonempty_string(), MnesiaError :: term()}}.
get_boxindices(Box, GroupNameList) when is_list(GroupNameList) ->
    gen_server:call({global, Box}, {get_boxindices, GroupNameList});
get_boxindices(Box, User) when is_record(User, ibo_user) ->
    get_boxindices(Box, User#ibo_user.groups).

%% retrieves the initialisation for the webinterface, which consists of a map with a schema, which is later converted to a json-schema
-spec get_webinit(Box :: binary(), XBOid :: binary()) -> { GroupName :: binary(), map()} | {error, not_found } | {error, MnesiaError :: term()}.
get_webinit(Box, XBOid) ->   % first ibo_xboline in ibo_xbostep to initialize the steps for the web-access
    gen_server:call({global, Box}, {get_webinit, XBOid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    Name = maps:get(name, Args),
    io:format("~p (~p) starting~n", [?MODULE, Name]),
    {ok, #state{domain = Name}}. % initial state

handle_call({process_xbo, XBO, StepNr}, _From, State) ->
    try check_xbo(XBO, StepNr, State) of
        ok ->
            {reply, store_xbo(XBO, StepNr), State}
    catch
        _:Error ->
            io:format("error processing xbo, check_xbo failed: ~p~n", [Error]),
            {reply, {error,{check_xbo,Error}}, State}
    end;
handle_call({get_boxindices, GroupNameList}, _From, State) ->
    {reply, read_boxindices(GroupNameList), State};
handle_call({get_webinit, XBOid}, _From, State) ->
    case db:read_transactional(ibo_boxdata, XBOid) of
        not_found ->
            {reply, {error,not_found}, State};
        {error, Reason} ->
            {reply, {error,Reason}, State};
        Boxdata ->
            Config = get_webinit_conf(Boxdata),
            {reply, xlib_box:webinit(Config), State} % TODO: make failsave -> server crashes when function webinit fails!
    end;
handle_call({execute_xbo, XBOid, DataMap}, From, State) ->
    case is_xbo_executing(XBOid, State) of
        true ->
            {reply, {error, already_started}};
        _Else ->
            case db:read_transactional(ibo_boxdata, XBOid) of
                not_found ->
                    {reply, {error,not_found}, State};
                {error, Reason} ->
                    {reply, {error,Reason}, State};
                Boxdata ->
                    %start subprocess that does the actual execution and the reply as well
                    Config = get_process_conf(Boxdata, DataMap),
                    Router = get_first_router(Boxdata),
                    Pid = spawn_link(?MODULE, xbo_childprocess, [Config, Router, From, State#state.domain]),
                    NewState = save_worker(Pid, Config, State),
                    {noreply, NewState}
            end
    end;
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info({'EXIT', Pid, normal}, State) ->
    %io:format("Trapped EXIT signal normal~n"),
    NewState = remove_worker(Pid, State),
    {noreply, NewState};
handle_info({'EXIT', Pid, _Reason}, State) ->
    %TODO: sub-process died and basic error handling did not work, so log error locally and remove xbo
    %io:format("Trapped EXIT signal with reason: ~p~n", [Reason]),
    NewState = remove_worker(Pid, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Executing XBO Childprocess
%%%===================================================================
xbo_childprocess(Config, Router, From, ServerName) ->
    io:format("xbo_childprocess started"),

    Result = try xlib:start(Config) of
        {finish, XlibState} ->
            xbo_childprocess_finish(Router, XlibState);
        {send, XlibState, NewStepNr, NewDestination} ->
            xbo_childprocess_send(Router, XlibState, NewStepNr, NewDestination);
        {error, XlibState, Reason} ->   % error detected by xlib, which returned error
            xbo_childprocess_error(Router, XlibState, Reason, ServerName)
    catch
        _:Error ->
            xbo_childprocess_error(Router, Config, Error, ServerName)
    end,
    gen_server:reply(From, Result),
    Result.

%removes the necessary data from the database, sends the xbo to the router and replies to the process which started the xbo execution
xbo_childprocess_finish(Router, XlibState) ->
    Res = mnesia:transaction(
        fun() ->
            remove_xbo_transactionless(XlibState),
            ok = xbo_router:end_xbo(Router, XlibState#xlib_state.xbo, XlibState#xlib_state.current_stepdata)
        end
    ),
    case Res of
        {atomic, ok} -> {ok, xbo_end};
        {aborted, Reason} -> {error, Reason}
    end.

xbo_childprocess_send(Router, XlibState, NewStepNr, NewDestination) ->
    Res = mnesia:transaction(
        fun() ->
            remove_xbo_transactionless(XlibState),
            ok = xbo_router:process_xbo(Router, XlibState#xlib_state.xbo, NewStepNr, XlibState#xlib_state.current_stepdata, NewDestination)
        end
    ),
    case Res of
        {atomic, ok} -> {ok, xbo_send};
        {aborted, Reason} -> {error, Reason}
    end.

xbo_childprocess_error(Router, XlibState, Reason, OwnName) ->
    Res = mnesia:transaction(
        fun() ->
            remove_xbo_transactionless(XlibState),
            xbo_router:debug_xbo(Router, XlibState, Reason, OwnName)
        end
    ),
    case Res of
        {atomic, ok} -> {error, xbo_error};
        {aborted, Reason} -> {error, Reason}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
remove_xbo_transactionless(XlibState) ->
    case mnesia:wread({ibo_boxdata, XlibState#xlib_state.xbo#ibo_xbo.id}) of
        [BoxData] ->
            mnesia:delete_object(BoxData),
            case mnesia:wread({ibo_boxindex, BoxData#ibo_boxdata.groupname}) of
                [R] ->
                    mnesia:write(R#ibo_boxindex{
                        xbolist = lists:dropwhile(fun(Element) -> Element#ibo_boxindex_elementpreview.xboid =:= BoxData#ibo_boxdata.xboid end, R#ibo_boxindex.xbolist)
                    });
                [] ->
                    mnesia:abort("particular mnesia boxindex has to exist but doesn't")
            end;
        [] ->
            mnesia:abort("particular mnesia boxdata has to exist but doesn't")
    end.

store_xbo(XBO, StepNr) ->   % TODO consider correlation ID to "merge" several IDs (correlation ID should be saved in step) -> correlation ID has to be set when creating XBO
    Step = lists:nth(StepNr, XBO#ibo_xbo.steps),
    GroupName = Step#ibo_xbostep.local,
    StepDescription = Step#ibo_xbostep.description,
    XBOid = XBO#ibo_xbo.id,
    XBOtemplate = XBO#ibo_xbo.template,

    % create new elements for the box and save them
    NewBoxRec = #ibo_boxdata{xboid = XBOid, xbodata = XBO, xbostepnr = StepNr, groupname = GroupName},
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
    throw_if_false(Step#ibo_xbostep.domain =:= State#state.domain, "Step is for a different domain"),

    XBOid = XBO#ibo_xbo.id,
    throw_if_true(XBOid =:= "", "Id of XBO must not be empty"),


    Line = lists:nth(1, Step#ibo_xbostep.commands),
    throw_if_false((Line#ibo_xboline.library =:= xlib_box andalso Line#ibo_xboline.command =:= webinit), "The first command must be webinit from the xlib_box library!"),
    case schema_validator:validate_schema(lists:nth(1, Line#ibo_xboline.args)) of
        {ok, _} ->
            ok;
        {error, {ReasonText, _}} ->
            throw("The first element in args of the first command must be a valid schema: " ++ ReasonText)
    end,

    % TODO check other commands as well

    % check if XBO already exists in database (=duplicate XBO) as last check -> slowest check
    throw_if_true(db:is_key_in_table(ibo_boxdata,XBOid), "XBO is already in Table"),
    ok.

read_boxindices(GroupNameList) when is_list(GroupNameList) ->
    Res = mnesia:transaction(
        fun() ->
            Q = qlc:q([R || R <- mnesia:table(ibo_boxindex),
                G <- GroupNameList, R#ibo_boxindex.groupname =:= G ]),
            qlc:e(Q)
        end),
    case Res of
        {atomic, X} when is_list(X) -> X;
        Err -> {error, {"Failed to retrieve boxindices", Err}}
    end.

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

get_webinit_conf(Boxdata) ->
    Stepdata = #ibo_xbostepdata{stepnr = Boxdata#ibo_boxdata.xbostepnr},
    #xlib_state{current_linenr = 1, xbo = Boxdata#ibo_boxdata.xbodata, current_stepdata = Stepdata}.

get_process_conf(Boxdata, DataMap) ->
    Stepdata = #ibo_xbostepdata{stepnr = Boxdata#ibo_boxdata.xbostepnr, vars = DataMap},
    #xlib_state{current_linenr = 2, xbo = Boxdata#ibo_boxdata.xbodata, current_stepdata = Stepdata}.

get_first_router(Boxdata) ->
    lists:nth(1, Boxdata#ibo_boxdata.xbodata#ibo_xbo.router).

save_worker(Pid, Config, State) ->
    State#state{workers = [{Pid, Config} |State#state.workers]}.

remove_worker(Pid, State) ->
    State#state{workers = lists:dropwhile(fun(Element) -> element(1, Element) =:= Pid end,State#state.workers)}.

is_xbo_executing(XBOid, State) ->
    lists:any(fun(Element) -> (element(2, Element))#xlib_state.xbo#ibo_xbo.id =:= XBOid end, State#state.workers).