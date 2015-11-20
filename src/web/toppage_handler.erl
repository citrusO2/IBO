%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2015 20:33
%%%-------------------------------------------------------------------
-module(toppage_handler).
-author("Florian").

%% API ---------------------------------------------------------------
-export([init/2]).

init(Req, Opts) ->
    Req2 = cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
    ], <<"Hello world!">>, Req),
    {ok, Req2, Opts}.
