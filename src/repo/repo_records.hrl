%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Dez 2015 21:03
%%%-------------------------------------------------------------------
-author("Florian").

-include("../xbo/xbo_records.hrl").
-include("../helper/type_specs.hrl").

-ifndef(REPO_RECORDS_HRL).
-define(REPO_RECORDS_HRL, 1).


-record(ibo_repo_gui_transition, {
    destination :: finish | non_neg_integer(),  % the destination of the step (either stepnr or
    vertices :: [point()]                       % points through which the transition line is drawn through
}).

-record(ibo_repo_gui_step, {
    position :: point(),                            % the position of the step in the gui (the size will be determined automatically)
    transitions :: list(#ibo_repo_gui_transition{}) % list of transition for the given step
}).

-record(ibo_repo_gui, {
    steps :: nonempty_list(#ibo_repo_gui_step{}),
    start :: {point(), [point()]},                  % position of the startpoint and its vertices
    endpoint :: point()                             % only the position, as the vertices are drawn via the steps
}).

-record(ibo_repo_template, {
    name :: binary(),                               % unique xbo template name
    description :: binary(),
    version :: non_neg_integer(),
    ttl :: non_neg_integer(),                       % timespan in seconds for how long the xbo is valid
    steps :: nonempty_list(#ibo_xbostep{}),
    groups :: nonempty_list(binary()),              % groups which may start the template
    startstepnr :: non_neg_integer(),
    startdestination :: binary(),
    created_by :: binary(),
    gui :: #ibo_repo_gui{},
    created_at = os:timestamp() :: timestamp(),
    transform = fun (XBO,_Args) -> XBO end :: fun( (#ibo_xbo{}, Args :: [] | any()) -> #ibo_xbo{})  % default = no transformation
}).

-record(ibo_repo_server, {
    name :: binary(),
    n = 1 :: non_neg_integer()
}).

-endif. % REPO_RECORDS_HRL defined