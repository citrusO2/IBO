%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2016 12:19
%%%-------------------------------------------------------------------
-module(general_handler).
-author("Florian").

-include("handler_macros.hrl").

-define(CACHE_TIME_SECS, 28800). % = 8h

%% API
-export([is_authorized/4, content_types_accepted/2, content_types_provided/2, set_session_cookie/2, delete_session_cookie/2]).

% State-Record must have the user element at the second place (=3rd place when considering the first element in the tuple as the atom for the record)
is_authorized(Req, State, DirectoryServerName, WebServerName) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        % if user asks for authorisation, try to authorize via basic
        {basic, UserID, Password} -> do_basic_auth(UserID, Password, Req, State, DirectoryServerName, WebServerName);

        % no authorisation in header, check if there is a session cookie
        _NoBasicAuth ->
            Cookies = cowboy_req:parse_cookies(Req),
            case lists:keyfind(<<"sessionid">>, 1, Cookies) of
                {_, SessionId} ->
                    case web_server:get_cached_user(WebServerName, SessionId) of
                        not_found ->
                            % session cookie supplied, but can't be found in cache
                            {{false, <<"BasicCustom realm=\"cowboy\"">>}, Req, State};
                        User ->
                            Req2 = case cowboy_req:binding(dir_type, Req) of
                                <<"logout">> -> delete_session_cookie(SessionId, Req);  % cookie needs to be overwritten on client-side to delete it
                                       _Else -> set_session_cookie(SessionId, Req)   % resetting the response cookie on the client side for every request
                            end,
                            {true, Req2, setelement(3, State, User)}
                    end;
                false ->
                    % no basic auth and no session cookie found => not authorized
                    {{false, <<"BasicCustom realm=\"cowboy\"">>}, Req, State}
            end
    end.

do_basic_auth(UserID, Password, Req, State, DirectoryServerName, WebServerName) ->
    try directory_server:get_user_info(DirectoryServerName, UserID,Password) of
        User when is_tuple(User) andalso element(1,User) =:= ibo_user ->
            case web_server:cache_user(WebServerName, User) of
                {error, _Message} ->
                    cowboy_req:reply(500, [{<<"content-type">>, <<"application/json">>}],<<"{\"error\": \"error when storing session data\"}">>, Req),
                    {stop, Req, State};
                SessionId ->
                    Req2 = set_session_cookie(SessionId, Req),
                    {true, Req2, setelement(3, State, User)}
            end;
        _ ->
            {{false, <<"BasicCustom realm=\"cowboy\"">>}, Req, State}   % BasicCustom instead of just Basic to avoid browser login window
    catch
        exit:{noproc,_} ->
            cowboy_req:reply(504, [{<<"content-type">>, <<"application/json">>}],<<"{\"error\": \"directory server could not be reached\"}">>, Req),
            {stop, Req, State}
    end.


% determines that the data from the server is of type application/json and json_get is used by cowboy as the function name to call
content_types_provided(Req, State) ->
    {[ {<<"application/json">>, json_get} ], Req, State}.

% determines that the data received by the server is of type application/json and json_post is used by cowboy as the function name to call
content_types_accepted(Req, State) ->
    {[ {{<<"application">>, <<"json">>, '*'}, json_post} ], Req, State}.


%%%===================================================================
%%% Helper
%%%===================================================================

set_session_cookie(SessionId, Req) ->
    cowboy_req:set_resp_cookie(<<"sessionid">>, SessionId, [{http_only, true}, {max_age, ?CACHE_TIME_SECS},  {path, "/api"}], Req).    %http only so no javascript can access it, also add {secure, true} when using https

% deletes session cookie by setting it's age to a time in the past
delete_session_cookie(SessionId, Req) ->
    cowboy_req:set_resp_cookie(<<"sessionid">>, SessionId, [{http_only, true}, {max_age, 0},  {path, "/api"}], Req).    %http only so no javascript can access it, also add {secure, true} when using https


%%create_session_id() ->
%%    %list_to_binary(to_hex(crypto:strong_rand_bytes(80))).
%%    base64:encode(crypto:strong_rand_bytes(80)).   % cookie must be string
%%
%%store_user_in_cache(SessionId, User) ->
%%    try insert_user(SessionId, User) of
%%        true -> true
%%    catch
%%        _:_->
%%            ets:new(?USER_CACHE, [set, named_table]),
%%            insert_user(SessionId, User)
%%    end.
%%
%%get_user_from_cache(SessionId) -> % not_found | User
%%    try ets:lookup(?USER_CACHE, SessionId) of
%%        [] ->
%%            io:format("lookup[]"),
%%            not_found;
%%        [{SessionId, {User, TimeStamp}}] ->
%%            io:format("lookup session user: ~p~n", [User]),
%%            io:format("lookup session timestamp: ~p~n", [TimeStamp]),
%%            Now = os:timestamp(),
%%            case timer:now_diff(Now, TimeStamp) of
%%                Difference when Difference >= ?CACHE_TIME_MSECS ->
%%                    io:format("Difference too much, delete: ~p~n", [Difference]),
%%                    ets:delete(?USER_CACHE, SessionId),
%%                    not_found;
%%                Difference when Difference < ?CACHE_TIME_MSECS ->
%%                    io:format("Difference enough, renew: ~p~n", [Difference]),
%%                    insert_user(SessionId, User),
%%                    User;
%%                Else ->
%%                    io:format("else: ~p~n", [TimeStamp]),
%%                    not_found
%%            end
%%    catch
%%        Exception:Message->
%%            io:format("lookup catch ~p, ~p, ~p, ~p~n", [Exception, Message, ?USER_CACHE, SessionId]),
%%            not_found
%%    end.
%%
%%insert_user(SessionId, User) ->
%%    ets:insert(?USER_CACHE,{SessionId, {User, os:timestamp()}}).