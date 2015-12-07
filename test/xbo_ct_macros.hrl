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
-define(XBO, #ibo_xbo{
    id = <<"1-141232">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"holidayapplication">>,
    steps = [#ibo_xbostep{
        domain = <<"box_server">>,
        local = <<"marketing">>,    % this XBO Step is meant for the "marketing" group (ibo_group.groupname)
        description = <<"Apply for your holidays">>,
        commands = [
            #ibo_xboline{
                library = test,
                command = testcommand,
                args = [miau]
            }
        ]
    }]
}).

-define(NEWXBO, #ibo_xbo{
    id = <<"1-141233">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    steps = [#ibo_xbostep{
        domain = <<"box_server">>,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = [
            #ibo_xboline{
                library = xlib_box,
                command = webinit,
                args = [
                    #{
                        description => <<"Approve the current marketing budget of 250.000€">>,
                        schema => #{
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
                        }
                    }
                ]
            }
        ]
    }]
}).

-define(FAILXBO_WRONGDOMAIN, #ibo_xbo{
    id = <<"1-141234">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"marketingbudgetdecision">>,
    steps = [#ibo_xbostep{
        domain = <<"star_destroyer">>,
        local = <<"marketing">>,
        description = <<"Accept or deny the marketing budget">>,
        commands = [
            #ibo_xboline{
                library = test,
                command = testcommand,
                args = [miau]
            }
        ]
    }]
}).

-define(NEWGROUPXBO1, #ibo_xbo{
    id = <<"1-141235">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"newvm">>,
    steps = [#ibo_xbostep{
        domain = <<"box_server">>,
        local = <<"it">>,
        description = <<"Create new VM">>,
        commands = [
            #ibo_xboline{
                library = test,
                command = testcommand,
                args = [miau]
            }
        ]
    }]
}).

-define(NEWGROUPXBO2, #ibo_xbo{
    id = <<"1-141236">>,
    format_indicator = 1,
    created_by = <<"hanswurst">>,
    template = <<"production_maintenance_planning">>,
    steps = [#ibo_xbostep{
        domain = <<"box_server">>,
        local = <<"production">>,
        description = <<"Approve or deny the maintenance plan">>,
        commands = [
            #ibo_xboline{
                library = test,
                command = testcommand,
                args = [miau]
            }
        ]
    }]
}).