%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dez 2015 21:27
%%%-------------------------------------------------------------------
-module(schema_validator_SUITE).
-author("Florian").

-include("schema_ct_macros.hrl").

%% Common Test Framework ---------------------------------------------
-include_lib("common_test/include/ct.hrl"). % enables ?config(Key, List) to retrieve properties from the Config
-export([all/0, init_per_testcase/2, end_per_testcase/2, init_per_suite/1, end_per_suite/1]).
-export([validate_schema_test/1]).

all() -> [validate_schema_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%%===================================================================
%%% Validation Tests
%%%===================================================================

validate_schema_test(_Config)->
    true = schema_validator:is_valid_schema(?TESTSCHEMA1),
    false = schema_validator:is_valid_schema(?TESTSCHEMA2),
    false = schema_validator:is_valid_schema(?TESTSCHEMA3),
    false = schema_validator:is_valid_schema(?TESTSCHEMA4),
    false = schema_validator:is_valid_schema(?TESTSCHEMA5),
    false = schema_validator:is_valid_schema(?TESTSCHEMA6),
    false = schema_validator:is_valid_schema(?TESTSCHEMA7),
    true = schema_validator:is_valid_schema(?TESTSCHEMA8),
    false = schema_validator:is_valid_schema(?TESTSCHEMA9),
    false = schema_validator:is_valid_schema(?TESTSCHEMA10),
    false = schema_validator:is_valid_schema(?TESTSCHEMA11).