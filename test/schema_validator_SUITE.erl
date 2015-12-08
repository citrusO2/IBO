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
-export([validate_schema_test/1,validate_schema_data_test/1]).

all() -> [validate_schema_test, validate_schema_data_test].

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

validate_schema_data_test(_Config)->
    Data1 = #{<<"reason">> => <<"DecisionReason">>, <<"yesno">> => <<"yes">>},
    true = schema_validator:is_valid_data(?TESTSCHEMA1,Data1),

    Data2 = #{<<"reason">> => <<"DecisionReason">>, <<"yesno">> => <<"haha">>},
    false = schema_validator:is_valid_data(?TESTSCHEMA1,Data2),

    Data3 = #{<<"yesno">> => <<"yes">>},
    false = schema_validator:is_valid_data(?TESTSCHEMA1,Data3),

    Data4 = #{<<"reason">> => <<"DecisionReason">>},
    false = schema_validator:is_valid_data(?TESTSCHEMA1,Data4),

    Data5 = #{<<"reason">> => 25, <<"yesno">> => <<"yes">>},
    false = schema_validator:is_valid_data(?TESTSCHEMA1,Data5),

    Data6 = #{<<"reason">> => true, <<"yesno">> => <<"yes">>},
    false = schema_validator:is_valid_data(?TESTSCHEMA1,Data6),

    Data7 = #{<<"reason">> => [<<"DecisionReason">>], <<"yesno">> => <<"yes">>},
    false = schema_validator:is_valid_data(?TESTSCHEMA1,Data7),

    Data8 = #{<<"reason">> => null, <<"yesno">> => <<"yes">>},
    false = schema_validator:is_valid_data(?TESTSCHEMA1,Data8),

    Data9 = #{<<"reason">> =>#{<<"properties">> => <<"DecisionReason">>}, <<"yesno">> => <<"yes">>},
    false = schema_validator:is_valid_data(?TESTSCHEMA1,Data9).