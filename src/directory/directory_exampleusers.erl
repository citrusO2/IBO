%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mär 2016 22:00
%%%-------------------------------------------------------------------
-module(directory_exampleusers).
-author("Florian").

%% API
-export([install/0]).

install() ->
    directory_server:create_user({ibo_user,<<"tu">>,<<"Theodor">>,<<"Urula">>,[<<"marketing">>,<<"admins">>],undefined},<<"pass123">>),
    directory_server:create_user({ibo_user,<<"mu">>,<<"Hanelore">>,<<"Marala">>,[<<"marketing_head">>,<<"admins">>],undefined},<<"pass123">>),
    directory_server:create_user({ibo_user,<<"fu">>,<<"Fabian">>,<<"Ustafa">>,[<<"it">>,<<"accounting">>],undefined},<<"pass123">>),
    directory_server:write_group({ibo_group,<<"acme">>,<<"Main Group of ACME Incorporated">>, undefined}),
    directory_server:write_group({ibo_group,<<"marketing">>,<<"Marketing Department Group">>, <<"acme">>}),
    directory_server:write_group({ibo_group,<<"it">>,<<"Marketing Department Group">>, <<"acme">>}),
    directory_server:write_group({ibo_group,<<"admins">>,<<"IT Department Group">>, <<"it">>}).