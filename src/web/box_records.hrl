%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Nov 2015 18:39
%%%-------------------------------------------------------------------
-author("Florian").

%% records -----------------------------------------------------------
%% field 1 = table name, field 2 = key, field 3 = field 1
%% saves the current XBO to retrieve it for later usage (so that user can access them)
-record(ibo_boxdata, {
    xboid :: nonempty_string(),
    xbodata :: any(),    % TODO replace with xbo-record after definition of xbo-record
    xbostepnr :: non_neg_integer()
}).

%% preview of the XBO which get shown to the user (so that not all XBOs have to retrieved all the time when accessing the box for a group)
-record(ibo_boxindex_elementpreview, {
    xboid :: nonempty_string(),
    xbotemplate :: nonempty_string(),
    xbostepdescription :: nonempty_string()     % maybe consider dynamically creating a description for each XBO (e.g. containing individual XBO information)
}).

%% index of all XBOs for an individual group, so that
-record(ibo_boxindex, {
    groupname :: nonempty_string(),
    xbolist = [] :: list(#ibo_boxindex_elementpreview{})
}).