%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     Repository server to store templates, which then get turned into XBOs and forwarded to the router
%%% @end
%%% Created : 30. Dez 2015 20:17
%%%-------------------------------------------------------------------
-module(repo_server).
-author("Florian").

-include("repo_records.hrl").
-include("../directory/directory_records.hrl").

%% repo_server internal state ----------------------------------------
-record(state, {
    router :: nonempty_list(nonempty_string()),
    deadletter :: nonempty_list(nonempty_string()),
    id_prefix :: nonempty_string(), % prefix before ID so that there is no clash when having several repo servers
    n :: non_neg_integer()          % running number for ID-Generation
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/1, stop/0, start_template/2, start_template/3, store_template/1]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
    gen_server:call(?MODULE, stop).

store_template(Template) when is_record(Template, ibo_repo_template) ->
    gen_server:call(?MODULE, {store_template, Template}).

start_template(User, TemplateName, Args) ->
    gen_server:call(?MODULE, {start_template, User, TemplateName, Args}).

start_template(User, TemplateName) ->
    start_template(User, TemplateName, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, create_state(Args)}.

handle_call({start_template, User, TemplateName, Args}, _From, State) ->
    case db:read_transactional(ibo_repo_template, TemplateName) of
        not_found ->
            {reply, {error, not_found}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State};
        Template ->
            create_xbo_response(Template, User, Args, State)
    end;
handle_call({store_template, Template}, _From, State) ->
    %% TODO: check repo template here
    {reply, db:write_transactional(Template), State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, N) -> {noreply, N}.
handle_info(_Info, N) -> {noreply, N}.
terminate(_Reason, _N) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.
code_change(_OldVsn, N, _Extra) -> {ok, N}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_xbo_response(Template, User, Args, State) -> %% TODO: put in a sub-process
    case is_any_listelement_in_list(User#ibo_user.groups, Template#ibo_repo_template.groups) of
        true ->
            XBO = create_xbo(Template, State, User),
            case transform_xbo(XBO, Template#ibo_repo_template.transform, Args) of
                {ok, NewXBO} ->
                    send_xbo_response(NewXBO, Template, State);
                {error, Msg} ->
                    {reply, {error, Msg}, State}
            end;
        false ->
            {reply, {error, "User " ++ User#ibo_user.username ++ " does not have access to the template: " ++ Template#ibo_repo_template.template}}
    end.

%% sends the xbo to the router and replies to the original sender
send_xbo_response(XBO, Template, State) ->
    Router = get_first_router(XBO),
    case erlang:apply(list_to_atom(Router), process_xbo, [XBO, Template#ibo_repo_template.startstepnr, Template#ibo_repo_template.startdestination]) of
        ok ->
            {reply, ok, State#state{n = State#state.n + 1}};
        Else ->
            {reply, Else, State}
    end.

transform_xbo(XBO, TransformFun, Args) ->
    case TransformFun(XBO, Args) of
        NewXBO when is_record(NewXBO, ibo_xbo) ->
            {ok, NewXBO};
        _Else ->
            {error, "XBO couldn't be transformed"}
    end.

create_xbo(Template, State, User) when is_record(Template, ibo_repo_template) ->
    CurrentTime = os:timestamp(),
    #ibo_xbo{
        id = list_to_binary(State#state.id_prefix ++ "-" ++ integer_to_list(State#state.n)),
        ttl = add_seconds_to_timestamp(CurrentTime, Template#ibo_repo_template.ttl),
        create_time = CurrentTime,
        created_by = User#ibo_user.username,
        template = Template#ibo_repo_template.template,
        template_version = Template#ibo_repo_template.template_version,
        router = State#state.router,    % TODO: scramble router list so that the XBOs get distributed across several router
        deadletter = State#state.deadletter,
        steps = Template#ibo_repo_template.steps}.

%%%===================================================================
%%% Helper functions
%%%===================================================================
create_state(Args) ->
    Router = element(1, Args),
    Deadletter = element(2, Args),
    Id_prefix = element(3, Args),
    N = element(4, Args),
    #state{router = Router, deadletter = Deadletter, id_prefix = Id_prefix, n = N}.

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

add_seconds_to_timestamp(Timestamp, Seconds) ->
    NewSeconds = now_to_seconds(Timestamp) + Seconds,
    {NewSeconds div 1000000, NewSeconds rem 1000000, 0}.

is_any_listelement_in_list(ListElements, List) ->
    lists:any(fun(Element) -> lists:member(Element, List) end, ListElements).

get_first_router(XBO) ->
    lists:nth(1, XBO#ibo_xbo.router).
