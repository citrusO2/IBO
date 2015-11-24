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
-record(ibo_boxdata, {
    xboid :: nonempty_string(),
    xbodata :: any(),    % TODO replace with xbo-record after definition of xbo-record
    xbostep :: non_neg_integer()
}).

-record(ibo_boxindex, {
    groupname :: nonempty_string(),
    xbolist = [] :: list(nonempty_string())
}).