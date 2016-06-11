%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2015 00:30
%%%-------------------------------------------------------------------
-author("Florian").

-include("../helper/type_specs.hrl").

-ifndef(DIRECTORY_RECORDS_HRL).
-define(DIRECTORY_RECORDS_HRL, 1).

%% records -----------------------------------------------------------
%% field 1 = table name, field 2 = key, field 3 = field 1
-record(ibo_user, {
    username :: binary(),
    firstname :: binary(),
    lastname :: binary(),
    groups = [] :: nonempty_list(binary()),
    access_to = [],    % list with all groups the user is a member of by inheritance, automatically generated
    password :: {byte(), byte()}    % salt + password
}).

-record(ibo_user_cache, {   % used to cache the recursive calls to resolve the access_to field from ibo_user
    username :: binary(),
    access_to = [] :: list(binary()),
    timestamp = os:timestamp() :: timestamp()
}).

-record(ibo_group, {
    name :: binary(),
    description :: binary(),
    parents = [] :: [binary()],         % direct parents of the group
    is_rulegroup = false :: boolean()   % boolean which determines if the group is a rule-group or a role-group
}).

-endif. % DIRECTORY_RECORDS_HRL defined