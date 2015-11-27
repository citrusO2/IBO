%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2015 00:30
%%%-------------------------------------------------------------------
-author("Florian").

%% records -----------------------------------------------------------
%% field 1 = table name, field 2 = key, field 3 = field 1
-record(ibo_user, {
    username :: nonempty_string(),
    firstname :: nonempty_string(),
    lastname :: nonempty_string(),
    groups = [] :: nonempty_list(nonempty_string()),
    password :: {byte(), byte()}    % salt + password
}).

-record(ibo_group, {
    groupname :: nonempty_string(),
    groupdescription :: nonempty_string(),
    parent :: string()
}).