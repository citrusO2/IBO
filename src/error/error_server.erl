%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     Server which stores XBOs with problems
%%%     TODO: Error server should also make it possible to fix erroneous IBOs and send it on its way again
%%% @end
%%% Created : 04. Jän 2016 06:33
%%%-------------------------------------------------------------------
-module(error_server).
-author("Florian").

-include("../xbo/xbo_records.hrl").
-include("../xlib/xlib_state.hrl").
-include("error_records.hrl").

%% error_server internal state ------------------------------------------
-record(state, {
    name :: binary()
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/1, stop/1, process_xbo/4, process_xbo/5]).

%% starts a new global error server with the given name as the global name
-spec start_link(Args :: #{name => binary()} ) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}
    ;           (Name :: binary()) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(Args) when is_map(Args) ->
    Name = maps:get(name, Args),
    gen_server:start_link({global, Name}, ?MODULE, Args, []);
start_link(Name) when is_binary(Name) ->
    start_link(#{name => Name}).

%% stops an error server by its name
-spec stop(Box :: binary()) -> ok.
stop(ErrorServerName) ->
    gen_server:call({global, ErrorServerName}, stop).

%% like other actors who deal with IBOs, the error server is only contacted from the router and also sends its packets back to the router
-spec process_xbo(ErrorServerName :: binary(), XBO :: #ibo_xbo{}, StepNr :: non_neg_integer(), Error :: term(), Destination :: binary()) -> ok | {error, term()}.
process_xbo(ErrorServerName, XBO, StepNr, Error, Destination) when is_record(XBO, ibo_xbo) ->    % store information about the XBOs problem (XBO itself, which step it was executing and what error was thrown
    process_xbo(
        ErrorServerName,
        #xlib_state{xbo = XBO, current_stepdata = #ibo_xbostepdata{stepnr = StepNr}},
        Error,
        Destination
    ).

-spec process_xbo(ErrorServerName :: binary(), XlibState :: #xlib_state{}, Error :: term(), Destination :: binary()) -> ok | {error, term()}.
process_xbo(ErrorServerName, XlibState, Error, Destination) when is_record(XlibState, xlib_state) ->
    gen_server:call({global, ErrorServerName}, {process_xbo, XlibState, Error, Destination}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    Name = maps:get(name, Args),
    io:format("~p (~p) starting~n", [?MODULE, Name]),
    create_tables_if_nonexistent(),
    {ok, #state{name = Name}}. % initial state

handle_call({process_xbo, XlibState, Error, Destination}, _From, State) ->
    {reply, store_case(XlibState, Error, Destination), State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, State) ->
    io:format("~p (~p) stopping~n", [?MODULE, State#state.name]),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_tables_if_nonexistent() ->
    db:create_local_table_if_nonexistent(ibo_errordata,
        record_info(fields, ibo_errordata),
        disc_copies, bag),
    ok = mnesia:wait_for_tables([ibo_errordata], 5000).

store_case(XlibState, Error, Destination) ->
    db:write_transactional(
    #ibo_errordata{
        xboid = XlibState#xlib_state.xbo#ibo_xbo.id,
        destination = Destination,
        error = Error,
        xlibstate = XlibState
    }).