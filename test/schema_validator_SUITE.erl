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
-export([validate_schema_test/1, validate_schema_data_test/1]).

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

validate_schema_test(_Config) ->
    TS1 = ?TESTSCHEMA1,
    TS2 = ?TESTSCHEMA2,
    TS3 = ?TESTSCHEMA3,
    TS4 = ?TESTSCHEMA4,
    TS5 = ?TESTSCHEMA5,
    TS6 = ?TESTSCHEMA6,
    TS7 = ?TESTSCHEMA7,
    TS8 = ?TESTSCHEMA8,
    TS9 = ?TESTSCHEMA9,
    TS10 = ?TESTSCHEMA10,
    TS11 = ?TESTSCHEMA11,

    {ok, TS1} = schema_validator:validate_schema(TS1),
    {error, {"description has to be of type binary", 2312}} = schema_validator:validate_schema(TS2),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS3}} = schema_validator:validate_schema(TS3),
    {error, {"title has to be of type binary", true}} = schema_validator:validate_schema(TS4),
    {ok, TS5} = schema_validator:validate_schema(TS5),
    {error, {"required elements must match the keys in properties", [<<"wuhaha">>, <<"yesno">>]}} = schema_validator:validate_schema(TS6),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS7}} = schema_validator:validate_schema(TS7),
    {ok, TS8} = schema_validator:validate_schema(TS8),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS9}} = schema_validator:validate_schema(TS9),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS10}} = schema_validator:validate_schema(TS10),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS11}} = schema_validator:validate_schema(TS11).

validate_schema_data_test(_Config) ->
    Data1 = #{<<"reason">> => <<"DecisionReason">>, <<"yesno">> => <<"yes">>},
    {ok, Data1} = schema_validator:validate_data(?TESTSCHEMA1, Data1),

    Data2 = #{<<"reason">> => <<"DecisionReason">>, <<"yesno">> => <<"haha">>},
    ResSchema2 = #{<<"description">> => <<"tick your decision">>,
        <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>],
        <<"title">> => <<"Decide">>,
        <<"type">> => <<"string">>},
    {error, {"The given value differs from the values in enum", {ResSchema2,
        <<"haha">>}}} = schema_validator:validate_data(?TESTSCHEMA1, Data2),

    Data3 = #{<<"yesno">> => <<"yes">>},
    {error, {"the given value-map does not contain all required keys", {[<<"reason">>, <<"yesno">>], Data3}}} = schema_validator:validate_data(?TESTSCHEMA1, Data3),

    Data4 = #{<<"reason">> => <<"DecisionReason">>},
    {error, {"the given value-map does not contain all required keys", {[<<"reason">>, <<"yesno">>], Data4}}} = schema_validator:validate_data(?TESTSCHEMA1, Data4),

    Data5 = #{<<"reason">> => 25, <<"yesno">> => <<"yes">>},
    ResSchema5 = #{<<"description">> => <<"The reason for your decision">>,
        <<"title">> => <<"Reason">>,
        <<"type">> => <<"string">>},
    {error, {"the given value is of type integer, but the schema is not of type integer or number", {ResSchema5, 25}}} = schema_validator:validate_data(?TESTSCHEMA1, Data5),

    ResSchema6 = ResSchema5,
    Data6 = #{<<"reason">> => true, <<"yesno">> => <<"yes">>},
    {error, {"the given value is of type boolean, but the schema is not of type boolean", {ResSchema6, true}}} = schema_validator:validate_data(?TESTSCHEMA1, Data6),

    ResSchema7 = ResSchema5,
    Data7 = #{<<"reason">> => [<<"DecisionReason">>], <<"yesno">> => <<"yes">>},
    {error, {"the given value is a list, but the schema is not of type array", {ResSchema7, [<<"DecisionReason">>]}}} = schema_validator:validate_data(?TESTSCHEMA1, Data7),

    ResSchema8 = ResSchema5,
    Data8 = #{<<"reason">> => null, <<"yesno">> => <<"yes">>},
    {error, {"the given value is of type null, but the schema is not of type null", {ResSchema8, null}}} = schema_validator:validate_data(?TESTSCHEMA1, Data8),

    Data9 = #{<<"reason">> =>#{<<"properties">> => <<"DecisionReason">>}, <<"yesno">> => <<"yes">>},
    Value9 = #{<<"properties">> => <<"DecisionReason">>},
    ResSchema9 = ResSchema5,
    {error, {"the given value is a map, but the schema is not of type object", {ResSchema9, Value9}}} = schema_validator:validate_data(?TESTSCHEMA1, Data9).