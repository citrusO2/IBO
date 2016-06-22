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

%% the Name stands for the globally registered name
-callback process_xbo(Name :: binary(), XBO :: #ibo_xbo{}, StepNr :: non_neg_integer()) -> ok | {error, any()}.

%% function which returns the allowed libraries and what libraries init-function is required for the initialisation. If no init is required, the map does not contain init
-callback xlib_info() -> #{libraries => [atom()], init => atom()} | #{libraries => [atom()]}.