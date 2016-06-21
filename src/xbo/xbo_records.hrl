%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     eXecutable Business Object
%%% @end
%%% Created : 24. Nov 2015 04:01
%%%-------------------------------------------------------------------
-author("Florian").

-include("../helper/type_specs.hrl").

-ifndef(XBO_RECORDS_HRL).
-define(XBO_RECORDS_HRL, 1).

-record(ibo_xboline, {
    library :: atom(),
    command :: atom(),
    args :: any()
}).

% represents an individual step executed at a specific machine
% the machine itself is not specified in the step! -> easier routing
-record(ibo_xbostep, {
    domain :: binary(),                % identifier for the system where the step should be executed
    local :: binary() | undefined,        % for possible further division (e.g. to deliver to the right input-box)
    description :: binary(),           % description of step
    commands :: nonempty_list(#ibo_xboline{})
}).

-record(ibo_xbostepdata, {
    create_time = os:timestamp() :: timestamp(),
    stepnr :: non_neg_integer(),
    vars = #{} :: #{}                               % variables stored in this step
}).

-record(ibo_xbo, {
    id :: nonempty_string(),                        % unique ID (every xbo-"starter" has it's own running ID, so that the IDs cannot collide)
    format_indicator = 1 :: non_neg_integer(),      % Version of the xbo/xbostep itself
    ttl :: timestamp(),                             % Maximum possible time in the system TODO: implement TTL
    create_time = os:timestamp() :: timestamp(),
    created_by :: nonempty_string(),                % username
    template :: nonempty_string(),                  % unique xbo template name
    template_version :: non_neg_integer(),
    router :: nonempty_list(nonempty_string()),     % list of router to use, first in list = mainrouter for the package
    error :: nonempty_list(nonempty_string()), % list of server in charge of handling erroneous XBOs
    steps :: nonempty_list(#ibo_xbostep{}),
    % correlations :: list({non_neg_integer(), nonempty_string()}),  % a list with stepnr and correlation ids to merge xbos
    % signature :: binary(),                        % signing xbo to prevent modification
    stepdata = [] :: list(#ibo_xbostepdata{})       % contains information provided by each step
}).

-endif. % XBO_RECORDS_HRL defined