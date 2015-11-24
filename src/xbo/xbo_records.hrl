%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     eXecutable Business Object
%%% @end
%%% Created : 24. Nov 2015 04:01
%%%-------------------------------------------------------------------
-author("Florian").

-type timestamp() :: {non_neg_integer(),non_neg_integer(), non_neg_integer() }.

-record(ibo_xboline, {
    library :: nonempty_string(),
    command :: nonempty_string(),
    args :: any()
}).

% represents an individual step executed at a specific machine
% the machine itself is not specified in the step! -> easier routing
-record(ibo_xbostep, {
    domain :: nonempty_string(),            % identifier for the system where the step should be executed
    local :: nonempty_string() | none(),    % for possible further division (e.g. to deliver to the right input-box)
    description :: nonempty_string(),       % description of step
    code :: nonempty_list(#ibo_xboline{})
}).

-record(ibo_xbo, {
    id :: nonempty_string(),                        % unique ID (every xbo-"starter" has it's own running ID, so that the IDs cannot collide)
    format_indicator = 1 :: non_neg_integer(),      % Version of the xbo/xbostep itself
    ttl :: timestamp(),                             % Maximum possible time in the system
    create_time = os:timestamp() :: timestamp(),
    created_by :: nonempty_string(),                % username
    template :: nonempty_string(),                  % unique xbo template name
    template_version :: non_neg_integer(),
    steps :: nonempty_list(#ibo_xbostep{}),
    % signature :: binary(),                        % signing xbo to prevent modification
    stepdata :: list()                             % contains information provided by each step
}).