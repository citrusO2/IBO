%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jän 2016 06:49
%%%-------------------------------------------------------------------
-author("Florian").

-include("../xbo/xbo_records.hrl").

-ifndef(DEADLETTER_RECORDS_HRL).
-define(DEADLETTER_RECORDS_HRL, 1).

%% became obsolete because deadletter_data are stored in a dets-file

%%-record(ibo_deadletterdata, {
%%    destination :: binary(),
%%    xbo :: #ibo_xbo{},
%%    newstepdata :: #ibo_xbostepdata{},
%%    newstep :: non_neg_integer()
%%}).

-endif. % DEADLETTER_RECORDS_HRL defined