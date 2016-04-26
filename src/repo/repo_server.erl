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

-include_lib("stdlib/include/qlc.hrl").
-include("repo_records.hrl").
-include("../directory/directory_records.hrl").

%% repo_server internal state ----------------------------------------
-record(state, {
    router :: nonempty_list(binary()),  % list of routers to use
    error :: nonempty_list(binary()),   % list of error handling servers
    name :: binary(),                   % name, which is also the prefix before the ID so that there is no clash when having several repo servers
    n :: non_neg_integer()              % running number for ID-Generation
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/1, stop/1, start_template/3, start_template/4, store_template/2, get_templatelist/2]).

%% starts a new global repo server with its name
-spec start_link(Args :: #{name => binary(), router => nonempty_list(binary()), error => nonempty_list(binary()), n => non_neg_integer()}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(Args) ->
    Name = maps:get(name, Args),
    gen_server:start_link({global, Name}, ?MODULE, Args, []).

%% stops a repo by its name
-spec stop(Repo :: binary()) -> ok.
stop(Repo) ->
    gen_server:call({global,Repo}, stop).

%% store a template in the repository
-spec store_template(Repo :: binary(), Template :: #ibo_repo_template{}) -> ok | {error, term()}.
store_template(Repo, Template) when is_record(Template, ibo_repo_template) ->
    gen_server:call({global,Repo}, {store_template, Template}).

%% starts the template determined by the template name, filled with the Args and checked if the user is allowed to execute it
-spec start_template(Repo :: binary(), User :: #ibo_user{}, TemplateName :: binary(), Args :: [] | any()) -> ok | {error, not_found} | {error, term()}.
start_template(Repo, User, TemplateName, Args) ->
    gen_server:call({global,Repo}, {start_template, User, TemplateName, Args}).

%% like start_template/3, just without Args
-spec start_template(Repo :: binary(), User :: #ibo_user{}, TemplateName :: binary()) -> ok | {error, not_found} | {error, term()}.
start_template(Repo, User, TemplateName) ->
    start_template(Repo, User, TemplateName, []).

%% gets a list of template for a particular user
-spec get_templatelist(Repo :: binary(), User :: #ibo_user{}) -> [] | [binary()] | {error, term()}.
get_templatelist(Repo, User) ->
    gen_server:call({global,Repo}, {get_templatelist, User}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    {ok, create_init_state(Args)}.   % TODO: persist running ID and only use running ID from Args when no persistent ID is available

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
handle_call({get_templatelist, User}, _From, State) ->
    {reply,get_templatelist_for_user(User), State};
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
get_templatelist_for_user(User) ->
    GroupNameList = User#ibo_user.groups,
    Res = mnesia:transaction(
        fun() ->
            Q = qlc:q([ R#ibo_repo_template.template || R <- mnesia:table(ibo_repo_template),
                G <- GroupNameList, lists:member(G, R#ibo_repo_template.groups)]),
            qlc:e(Q)
        end),
    case Res of
        {atomic, X} when is_list(X) -> X;
        Err -> {error, {"Failed to retrieve templates", Err}}
    end.

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
    case xbo_router:process_xbo(XBO, Template#ibo_repo_template.startstepnr, Template#ibo_repo_template.startdestination) of
        ok ->
            {reply, ok, State#state{n = State#state.n + 1}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
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
        id = binary:list_to_bin([State#state.name, <<"-">>, integer_to_list(State#state.n)]),
        ttl = add_seconds_to_timestamp(CurrentTime, Template#ibo_repo_template.ttl),
        create_time = CurrentTime,
        created_by = User#ibo_user.username,
        template = Template#ibo_repo_template.template,
        template_version = Template#ibo_repo_template.template_version,
        router = State#state.router,    % TODO: scramble router list so that the XBOs get distributed across several router
        error = State#state.error,
        steps = Template#ibo_repo_template.steps}.

%%%===================================================================
%%% Helper functions
%%%===================================================================
create_init_state(Args) ->
    #state{router = maps:get(router, Args), error = maps:get(error, Args), name = maps:get(name, Args), n = maps:get(n, Args)}.

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

add_seconds_to_timestamp(Timestamp, Seconds) ->
    NewSeconds = now_to_seconds(Timestamp) + Seconds,
    {NewSeconds div 1000000, NewSeconds rem 1000000, 0}.

is_any_listelement_in_list(ListElements, List) ->
    lists:any(fun(Element) -> lists:member(Element, List) end, ListElements).
