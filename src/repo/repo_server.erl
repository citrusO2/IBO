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
    router :: nonempty_list(binary()),      % list of routers to use
    error :: nonempty_list(binary()),       % list of error handling servers
    name :: binary(),                       % name, which is also the prefix before the ID so that there is no clash when having several repo servers
    n :: non_neg_integer(),                 % running number for ID-Generation
    managegroups :: nonempty_list(binary()) % list of groups who are allowed to store templates
}).

%% gen_server --------------------------------------------------------
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

%% API ---------------------------------------------------------------
-export([start_link/1, stop/1, start_template/4, start_template/5, store_template/3, get_templatelist/2]).

%% starts a new global repo server with its name
-spec start_link(Args :: #{name => binary(), router => nonempty_list(binary()), error => nonempty_list(binary()), n => non_neg_integer(), managegroups => [binary()]}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.
start_link(Args) ->
    Name = maps:get(name, Args),
    gen_server:start_link({global, Name}, ?MODULE, Args, []).

%% stops a repo by its name
-spec stop(Repo :: binary()) -> ok.
stop(Repo) ->
    gen_server:call({global,Repo}, stop).

%% store a template in the repository
-spec store_template(Repo :: binary(), Template :: #ibo_repo_template{}, Groups :: [binary()]) -> ok | {error, term()}.
store_template(Repo, Template, Groups) when is_record(Template, ibo_repo_template), is_list(Groups) ->
    gen_server:call({global,Repo}, {store_template, Template, Groups}).

%% starts the template determined by the template name, filled with the Args and checked if one of the given groups is allowed to execute it
-spec start_template(Repo :: binary(), Groups :: [binary()], Creator :: binary(), TemplateName :: binary(), Args :: [] | any()) -> ok | {error, not_found} | {error, term()}.
start_template(Repo, Groups, Creator, TemplateName, Args) when is_list(Groups)->
    gen_server:call({global,Repo}, {start_template, Groups, Creator, TemplateName, Args}).

%% like start_template/4, just without Args
-spec start_template(Repo :: binary(), Groups :: [binary()], Creator :: binary(), TemplateName :: binary()) -> ok | {error, not_found} | {error, term()}.
start_template(Repo, Groups, Creator, TemplateName) ->
    start_template(Repo, Groups, Creator, TemplateName, []).

%% gets a list of templates for the given groups
-spec get_templatelist(Repo :: binary(), Groups :: [binary()]) -> [] | [binary()] | {error, term()}.
get_templatelist(Repo, Groups) when is_list(Groups)->
    gen_server:call({global,Repo}, {get_templatelist, Groups}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(Args) ->
    process_flag(trap_exit, true), % to call terminate/2 when the application is stopped
    io:format("~p starting~n", [?MODULE]),
    create_tables_if_nonexistent(),
    {ok, helper:map_to_server_state_strict(Args,record_info(fields, state))}.   % TODO: persist running ID and only use running ID from Args when no persistent ID is available

handle_call({start_template, Groups, Creator, TemplateName, Args}, _From, State) ->
    case db:read_transactional(ibo_repo_template, TemplateName) of
        not_found ->
            {reply, {error, not_found}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State};
        Template ->
            create_xbo_response(Template, Groups, Creator, Args, State)
    end;
handle_call({store_template, Template, Groups}, _From, State) ->
    Reply = case helper:has_group_permission(Groups, State#state.managegroups) of
        true ->
            store_and_backup_template(Template);
        false ->
            {error, "No group of the given groups (" ++ helper:binary_list_to_string(Groups, <<", ">>) ++ ") is allowed to store templates"}
    end,
    {reply, Reply, State};
handle_call({get_templatelist, Groups}, _From, State) ->
    {reply, get_templatelist_for_groups(Groups), State};
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
create_tables_if_nonexistent() ->
    db:create_local_table_if_nonexistent(ibo_repo_template,
        record_info(fields, ibo_repo_template),
        disc_copies, set),
    db:create_local_table_if_nonexistent(ibo_repo_template_old,
        record_info(fields, ibo_repo_template),
        disc_only_copies, bag), % disc only, as holding also the old templates in ram is unnecessary
    ok = mnesia:wait_for_tables([ibo_repo_template, ibo_repo_template_old], 5000).

store_and_backup_template(Template) ->
    Res = mnesia:transaction(
        fun() ->
            case mnesia:wread({ibo_repo_template, Template#ibo_repo_template.name}) of
                [R] ->  % there is already a template with the same name
                    mnesia:write(setelement(1,R,ibo_repo_template_old)),   % write old repo template to the other table
                    mnesia:write(Template#ibo_repo_template{version = R#ibo_repo_template.version + 1});
                [] ->
                    mnesia:write(Template#ibo_repo_template{version = 1})
            end
        end),
    case Res of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

get_templatelist_for_groups(Groups) ->
    Res = mnesia:transaction(
        fun() ->
            Q = qlc:q([ R#ibo_repo_template.name || R <- mnesia:table(ibo_repo_template),
                G <- Groups, lists:member(G, R#ibo_repo_template.groups)]),
            qlc:e(Q)
        end),
    case Res of
        {atomic, X} when is_list(X) -> X;
        Err -> {error, {"Failed to retrieve templates", Err}}
    end.

create_xbo_response(Template, Groups, Creator, Args, State) -> %% TODO: put in a sub-process
    case helper:has_group_permission(Groups, Template#ibo_repo_template.groups) of
        true ->
            XBO = create_xbo(Template, State, Creator),
            case transform_xbo(XBO, Template#ibo_repo_template.transform, Args) of
                {ok, NewXBO} ->
                    send_xbo_response(NewXBO, Template, State);
                {error, Msg} ->
                    {reply, {error, Msg}, State}
            end;
        false ->
            {reply, {error, "No group of the given groups (" ++ helper:binary_list_to_string(Groups, <<", ">>) ++ ") has access to the template: " ++ Template#ibo_repo_template.name}, State}
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

create_xbo(Template, State, Creator) when is_record(Template, ibo_repo_template) ->
    CurrentTime = os:timestamp(),
    #ibo_xbo{
        id = binary:list_to_bin([State#state.name, <<"-">>, integer_to_list(State#state.n)]),
        ttl = add_seconds_to_timestamp(CurrentTime, Template#ibo_repo_template.ttl),
        create_time = CurrentTime,
        created_by = Creator,
        template = Template#ibo_repo_template.name,
        template_version = Template#ibo_repo_template.version,
        router = State#state.router,    % TODO: scramble router list so that the XBOs get distributed across several router
        error = State#state.error,
        steps = Template#ibo_repo_template.steps}.

%%%===================================================================
%%% Helper functions
%%%===================================================================
%%create_init_state(Args) ->
%%    #state{router = maps:get(router, Args), error = maps:get(error, Args), name = maps:get(name, Args), n = maps:get(n, Args), managegroups = maps:get(managegroups, Args)}.

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

add_seconds_to_timestamp(Timestamp, Seconds) ->
    NewSeconds = now_to_seconds(Timestamp) + Seconds,
    {NewSeconds div 1000000, NewSeconds rem 1000000, 0}.

%%is_any_listelement_in_list(ListElements, List) ->
%%    lists:any(fun(Element) -> lists:member(Element, List) end, ListElements).
%%
%%
