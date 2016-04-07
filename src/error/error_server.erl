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

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API
-export([start_link/0, stop/0, process_xbo/3, process_xbo/2]).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call({global, ?MODULE}, stop).

process_xbo(XBO, StepNr, Error) when is_record(XBO, ibo_xbo) ->    % store information about the XBOs problem (XBO itself, which step it was executing and what error was thrown
    process_xbo(
        #xlib_state{xbo = XBO, current_stepdata = #ibo_xbostepdata{stepnr = StepNr}},
        Error
    ).

process_xbo(XlibState, Error) when is_record(XlibState, xlib_state) ->
    gen_server:call({global, ?MODULE}, {process_xbo, XlibState, Error}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({process_xbo, XlibState, Error}, _From, State) ->
    {reply, store_case(XlibState, Error), State};
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
store_case(XlibState, Error) ->
    store_case(#ibo_errorcase{
        xlibstate = XlibState,
        error = Error
    }).

store_case(Case) when is_record(Case, ibo_errorcase) ->
    Id = Case#ibo_errorcase.xlibstate#xlib_state.xbo#ibo_xbo.id,
    Res = mnesia:transaction(
        fun() ->
            case mnesia:wread({ibo_errordata, Id}) of
                [R] ->
                    mnesia:write(R#ibo_errordata{
                        cases = [Case | R#ibo_errordata.cases]
                    });
                [] ->
                    mnesia:write(#ibo_errordata{xboid = Id, cases = [Case]})
            end
        end
    ),
    case Res of
        {atomic, ok} -> ok;
        _ -> {error, "Write failure"}
    end.