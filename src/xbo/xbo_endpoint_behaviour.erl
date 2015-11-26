%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Nov 2015 12:37
%%%-------------------------------------------------------------------
-module(xbo_endpoint_behaviour).
-author("Florian").

%% API
-include("xbo_records.hrl").
-callback process_xbo(XBO :: #ibo_xbo{}, StepNr :: non_neg_integer()) -> ok | {error, any()}.
