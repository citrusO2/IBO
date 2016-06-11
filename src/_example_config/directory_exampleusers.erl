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

-include("../directory/directory_records.hrl").

%% API
-export([install/1]).

install(Directory) ->
    % rule groups
    ok = directory_server:write_group(Directory, #ibo_group{name = <<"ACL">>, description = <<"Access Control List Main Group (Rule Group)">>, is_rulegroup = true}),
    ok = directory_server:write_group(Directory, #ibo_group{name = <<"ACL_Allow_Template_Storing">>, description = <<"Groups allows the storing of templates (Rule Group)">>, parents = [<<"ACL">>], is_rulegroup = true}),

    % role groups
    ok = directory_server:write_group(Directory, #ibo_group{name = <<"acme">>, description = <<"Main Group of ACME Incorporated">>}),
    ok = directory_server:write_group(Directory, #ibo_group{name = <<"marketing">>, description = <<"Marketing Department Group">>, parents = [<<"acme">>]}),
    ok = directory_server:write_group(Directory, #ibo_group{name = <<"it">>, description = <<"IT Department Group">>, parents = [<<"acme">>]}),
    ok = directory_server:write_group(Directory, #ibo_group{name = <<"it_admins">>, description = <<"IT Department's Admin Group">>, parents = [<<"it">>, <<"ACL_Allow_Template_Storing">>]}),

    % users
    ok = directory_server:create_user(Directory, #ibo_user{username = <<"tu">>, firstname = <<"Theodor">>, lastname = <<"Urula">>, groups =  [<<"marketing">>,<<"it_admins">>], password = <<"pass123">>}),
    ok = directory_server:create_user(Directory, #ibo_user{username = <<"fu">>, firstname = <<"Fabian">>, lastname = <<"Ustafa">>, groups = [<<"it">>], password = <<"pass123">>}).