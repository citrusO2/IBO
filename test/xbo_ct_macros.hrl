%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2015 11:23
%%%-------------------------------------------------------------------
-author("Florian").

-include("../src/box/box_records.hrl").

-define(BOX_NAME, <<"BOX1">>).
-define(ROUTER_NAME, <<"ROUTER1">>).
-define(ROUTER2_NAME, <<"ROUTER2">>).
-define(ERROR_SERVER_NAME, <<"ERRORSRV1">>).

-define(XBO_COMMANDS, [
    #ibo_xboline{
        library = xlib_box,
        command = webinit,
        args = [
            #{
                <<"title">> => <<"Marketing Budget - Decision">>,
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
                        <<"enum">> => [<<"no">>,<<"yes">>,<<"maybe">>]
                    }
                },
                <<"required">> => [<<"reason">>, <<"yesno">>]
            }
        ]
    },#ibo_xboline{library = xlib, command = finish}
]).

-define(XBO_COMMANDS2, [
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
                        <<"enum">> => [<<"no">>,<<"yes">>,<<"maybe">>]
                    }
                },
                <<"required">> => [<<"reason">>, <<"yesno">>]
            }
        ]
    },#ibo_xboline{library = xlib, command = cjump, args = [4, fun(StepData, OtherStepData) -> case maps:find(<<"yesno">>, StepData#ibo_xbostepdata.vars) of {ok, <<"yes">>} -> true; _Else -> false end end]},
    #ibo_xboline{library = xlib, command = send, args = [1, ?BOX_NAME]},
    #ibo_xboline{library = xlib, command = finish}
]).

-define(XBO_COMMANDS3, [
    #ibo_xboline{
        library = xlib_box,
        command = webinit,
        args = [
            #{
                <<"title">> => <<"Marketing Budget - Decision">>,
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
                        <<"enum">> => [<<"no">>,<<"yes">>,<<"maybe">>]
                    }
                },
                <<"required">> => [<<"reason">>, <<"yesno">>]
            },fun(Taskdetailstemplate, OtherStepdata) -> Taskdetailstemplate#{<<"description">> := <<"Wuhu, replaced task description">>} end
        ]
    },#ibo_xboline{library = xlib, command = finish}
]).

-define(XBO, #ibo_xbo{
    id = <<"1-141232">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"holidayapplication">>,
    router = [?ROUTER_NAME, ?ROUTER2_NAME],
    steps = [#ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"marketing">>,    % this XBO Step is meant for the "marketing" group (ibo_group.groupname)
        description = <<"Apply for your holidays">>,
        commands = ?XBO_COMMANDS
    }]
}).

-define(NEWXBO, #ibo_xbo{
    id = <<"1-141233">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    router = [?ROUTER_NAME],
    error =  [?ERROR_SERVER_NAME],
    steps = [#ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = ?XBO_COMMANDS
    }]
}).

-define(NEWXBO2, #ibo_xbo{
    id = <<"1-141233">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    router = [?ROUTER2_NAME],
    error =  [?ERROR_SERVER_NAME],
    steps = [#ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = ?XBO_COMMANDS
    }]
}).

-define(NEWXBO3, #ibo_xbo{
    id = <<"1-141233">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    router = [?ROUTER_NAME, ?ROUTER2_NAME],
    error =  [?ERROR_SERVER_NAME],
    steps = [#ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = ?XBO_COMMANDS
    }]
}).

-define(DYNWEBINITTESTXBO, #ibo_xbo{
    id = <<"1-141233">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    router = [?ROUTER_NAME],
    steps = [#ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = ?XBO_COMMANDS3
    }]
}).

-define(LIBTEST1XBO, #ibo_xbo{
    id = <<"1-141233">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    router = [?ROUTER_NAME],
    steps = [#ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = ?XBO_COMMANDS2
    }]
}).

-define(FAILXBO_WRONGDOMAIN, #ibo_xbo{
    id = <<"1-141234">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    error = [?ERROR_SERVER_NAME],
    router = [?ROUTER_NAME],
    steps = [#ibo_xbostep{
        domain = <<"star_destroyer">>,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = ?XBO_COMMANDS
    }]
}).

-define(NEWGROUPXBO1, #ibo_xbo{
    id = <<"1-141235">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"newvm">>,
    steps = [#ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"it">>,
        description = <<"Create new VM">>,
        commands = ?XBO_COMMANDS
    }]
}).

-define(NEWGROUPXBO2, #ibo_xbo{
    id = <<"1-141236">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"production_maintenance_planning">>,
    steps = [#ibo_xbostep{
        domain = ?BOX_NAME,
        local = <<"production">>,
        description = <<"Approve or deny the maintenance plan">>,
        commands = ?XBO_COMMANDS
    }]
}).

-define(LIBTEST2XBO, #ibo_xbo{
    id = <<"1-141233">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    router = [?ROUTER_NAME],
    steps = [#ibo_xbostep{
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
        ]
    }, #ibo_xbostep{
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
    ]
}).