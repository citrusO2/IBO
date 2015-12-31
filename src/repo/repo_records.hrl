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

-ifndef(REPO_RECORDS_HRL).
-define(REPO_RECORDS_HRL, 1).

-record(ibo_repo_template, {
    template :: nonempty_string(),                  % unique xbo template name
    template_version :: non_neg_integer(),
    ttl :: non_neg_integer(),                       % timespan in seconds for how long the xbo is valid
    steps :: nonempty_list(#ibo_xbostep{}),
    groups :: nonempty_list(nonempty_string()),     % groups which may start the template
    startstepnr :: non_neg_integer(),
    startdestination :: nonempty_string(),
    transform :: fun( (#ibo_xbo{}, Args :: [] | any()) -> #ibo_xbo{})
}).

-endif. % REPO_RECORDS_HRL defined