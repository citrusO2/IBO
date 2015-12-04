%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Dez 2015 14:27
%%%-------------------------------------------------------------------
-module(box_handler).
-author("Florian").

-include("../directory/directory_records.hrl").

%% API ---------------------------------------------------------------
-export([init/2]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([to_text/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

%% Authentication ----------------------------------------------------
is_authorized(Req, State) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, UserID, Password} ->
            case directory_server:get_user_info(UserID,Password) of
                User when is_tuple(User) andalso element(1,User) =:= ibo_user ->
                    {true, Req, User};
                {error, Reason} ->
                    {{false, <<"Basic realm=\"cowboy\", ", (list_to_binary(Reason))/binary>>}, Req, State};
                _ ->
                    {{false, <<"Basic realm=\"cowboy\", unknown error">>}, Req, State}
            end;
        _ ->
            {{false, <<"Basic realm=\"cowboy\", authorization missing in header">>}, Req, State}
    end.


%% ------------------------
content_types_provided(Req, State) ->
    {[
        {<<"text/plain">>, to_text}
    ], Req, State}.

to_text(Req, User) ->
    {<<"Hello, ", (User#ibo_user.firstname)/binary, "!\n">>, Req, User}.