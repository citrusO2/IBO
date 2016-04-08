%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dez 2015 20:13
%%%-------------------------------------------------------------------
-author("Florian").

-include("../src/xbo/xbo_records.hrl").
-include("../src/repo/repo_records.hrl").
-include("../src/directory/directory_records.hrl").

-define(BOX_NAME, <<"BOX1">>).
-define(ROUTER_NAME, <<"ROUTER1">>).
-define(REPO_NAME, <<"REPO1">>).

-define(MARKETINGUSER, #ibo_user{username = <<"dd">>, firstname = <<"Doris">>, lastname = <<"Dührwald">>, groups = [<<"marketing">>]}).

-define(TEMPLATE_TESTSTEPS1, [
    #ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = [
            #ibo_xboline{
                library = xlib_box,
                command = webinit,
                args = [
                    #{
                        <<"title">> => <<"Marketing Budget - Decision2">>,
                        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"reason">> => #{
                                <<"title">> => <<"Reason">>,
                                <<"description">> => <<"The reason for your decision">>,
                                <<"type">> => <<"string">>
                            },
                            <<"yesno">> => #{
                                <<"title">> => <<"Decide">>,
                                <<"description">> => <<"tick your decision">>,
                                <<"type">> => <<"string">>,
                                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
                            }
                        },
                        <<"required">> => [<<"reason">>, <<"yesno">>]
                    }
                ]
            }, #ibo_xboline{library = xlib, command = cjump, args = [4, fun(StepData, OtherStepData) ->
                case maps:find(<<"yesno">>, StepData#ibo_xbostepdata.vars) of {ok, <<"yes">>} -> true; _Else ->
                    false end end]},
            #ibo_xboline{library = xlib, command = send, args = [1, "box_server"]},
            #ibo_xboline{library = xlib, command = send, args = [2, "box_server"]}
        ]},
    #ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = [#ibo_xboline{
            library = xlib_box,
            command = webinit,
            args = [
                #{
                    <<"title">> => <<"Marketing Budget - Decision approved">>,
                    <<"description">> => <<"Information that the Marketing Budget was approved">>,
                    <<"type">> => <<"object">>,
                    <<"properties">> => #{
                        <<"ok">> => #{
                            <<"title">> => <<"ok">>,
                            <<"description">> => <<"confirm receive of message">>,
                            <<"type">> => <<"string">>,
                            <<"enum">> => [<<"ok">>]
                        }
                    },
                    <<"required">> => [<<"ok">>]
                }
            ]
        }, #ibo_xboline{library = xlib, command = finish}
        ]
    }
]).

-define(TEMPLATE_TESTTEMPLATE1,
    #ibo_repo_template{
        startdestination = ?BOX_NAME,
        startstepnr = 1,
        steps = ?TEMPLATE_TESTSTEPS1,
        groups = [<<"marketing">>],
        template = <<"marketingbudgetdecision">>,
        template_version = 1,
        ttl = 60*60*24*7,   % = 1 week
        transform = fun (XBO,_Args) -> XBO end  % = no change
    }
).

-define(TEMPLATE_TESTTEMPLATE2,
    #ibo_repo_template{
        startdestination = ?BOX_NAME,
        startstepnr = 1,
        steps = ?TEMPLATE_TESTSTEPS1,
        groups = [<<"marketing">>, <<"maybes">>],
        template = <<"malala">>,
        template_version = 1,
        ttl = 60*60*24*7,   % = 1 week
        transform = fun (XBO,_Args) -> XBO end  % = no change
    }
).

-define(TEMPLATE_TESTTEMPLATE3,
    #ibo_repo_template{
        startdestination = ?BOX_NAME,
        startstepnr = 1,
        steps = ?TEMPLATE_TESTSTEPS1,
        groups = [<<"it">>],
        template = <<"wuhaha">>,
        template_version = 1,
        ttl = 60*60*24*7,   % = 1 week
        transform = fun (XBO,_Args) -> XBO end  % = no change
    }
).