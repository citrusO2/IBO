%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2016 04:29
%%%-------------------------------------------------------------------
-module(watchdog_exampleconfig).
-author("Florian").

%% API
-export([install/0]).

install() ->
    Directory = <<"DIRECTORY1">>,
    ok = watchdog_server:start_iactor(directory_sup, Directory),
    Error = <<"ERROR1">>,
    ok = watchdog_server:start_iactor(error_sup, Error),
    Box = <<"BOX1">>,
    ok = watchdog_server:start_iactor(box_sup, Box),
    Router = <<"ROUTER1">>,
    RouterArgs = #{name => Router, allowed => [Box]},
    ok = watchdog_server:start_iactor(xbo_router_sup, RouterArgs),
    Repo = <<"REPO1">>,
    RepoArgs = #{name => Repo, router => [Router], error => [Error], n=>1, managegroups =>[<<"ACL_Allow_Template_Storing">>]},
    ok = watchdog_server:start_iactor(repo_sup, RepoArgs),
    Web = <<"WEB1">>,
    WebArgs = #{name => Web, directory => Directory, box => Box, repo => Repo},
    ok = watchdog_server:start_iactor(web_sup, WebArgs).