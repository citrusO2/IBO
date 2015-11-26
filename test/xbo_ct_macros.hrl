%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2015 11:23
%%%-------------------------------------------------------------------
-author("Florian").

-include("../src/web/box_records.hrl").
-define(XBO, #ibo_xbo{
    id = "1-141232",
    format_indicator = 1,
    created_by = "hanswurst",
    template = "holidayapplication",
    steps = [#ibo_xbostep{
        domain = "box_server",
        local = "marketing",    % this XBO Step is meant for the "marketing" group (ibo_group.groupname)
        commands = [
            #ibo_xboline{
                library = "TEST",
                command = "testcommand",
                args = "testarg"
            }
        ]
    }]
}).

-define(NEWXBO, #ibo_xbo{
    id = "1-141233",
    format_indicator = 1,
    created_by = "hanswurst",
    template = "marketingbudgetdecision",
    steps = [#ibo_xbostep{
        domain = "box_server",
        local = "marketing",
        commands = [
            #ibo_xboline{
                library = "TEST",
                command = "testcommand",
                args = "testarg"
            }
        ]
    }]
}).

-define(FAILXBO_WRONGDOMAIN, #ibo_xbo{
    id = "1-141234",
    format_indicator = 1,
    created_by = "hanswurst",
    template = "marketingbudgetdecision",
    steps = [#ibo_xbostep{
        domain = "star_destroyer",
        local = "marketing",
        commands = [
            #ibo_xboline{
                library = "TEST",
                command = "testcommand",
                args = "testarg"
            }
        ]
    }]
}).