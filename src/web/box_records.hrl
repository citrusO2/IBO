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
    iboid :: nonempty_string(),
    ibodata :: any(),    % TODO replace with ibo-record after definition of ibo-record
    ibostep :: non_neg_integer()
}).

-record(ibo_boxindex, {
    groupname :: nonempty_string(),
    ibolist = [] :: list(nonempty_string())
}).