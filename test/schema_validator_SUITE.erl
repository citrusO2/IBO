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
-export([validate_schema_test/1, validate_schema_data_test/1, validate_json_schema_testsuite/1]).

all() -> [validate_schema_test, validate_schema_data_test, validate_json_schema_testsuite].

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
    TS12 = ?TESTSCHEMA12,
    TS13 = ?TESTSCHEMA13,

    {ok, TS1} = schema_validator:validate_schema(TS1),
    {error, {"description must be of type binary", 2312}} = schema_validator:validate_schema(TS2),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS3}} = schema_validator:validate_schema(TS3),
    {error, {"title must be of type binary", true}} = schema_validator:validate_schema(TS4),
    {ok, TS5} = schema_validator:validate_schema(TS5),
    {error, {"required elements must match the keys in properties", [<<"wuhaha">>, <<"yesno">>]}} = schema_validator:validate_schema(TS6),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS7}} = schema_validator:validate_schema(TS7),
    {ok, TS8} = schema_validator:validate_schema(TS8),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS9}} = schema_validator:validate_schema(TS9),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS10}} = schema_validator:validate_schema(TS10),
    {error, {"the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", TS11}} = schema_validator:validate_schema(TS11),
    {ok, TS12} = schema_validator:validate_schema(TS12),
    {ok, TS13} = schema_validator:validate_schema(TS13).

validate_schema_data_test(_Config) ->
    Data1 = #{<<"reason">> => <<"DecisionReason">>, <<"yesno">> => <<"yes">>},
    {ok, Data1} = schema_validator:validate_data(?TESTSCHEMA1, Data1),

    Data2 = #{<<"reason">> => <<"DecisionReason">>, <<"yesno">> => <<"haha">>},
    ResSchema2 = [<<"no">>,<<"yes">>,<<"maybe">>],
    {error, {"field \"enum\" requires certain values", {ResSchema2,
        <<"haha">>}}} = schema_validator:validate_data(?TESTSCHEMA1, Data2),

    Data3 = #{<<"yesno">> => <<"yes">>},
    {error, {"field \"required\" requires certain keys to exist", {[<<"reason">>, <<"yesno">>], Data3}}} = schema_validator:validate_data(?TESTSCHEMA1, Data3),

    Data4 = #{<<"reason">> => <<"DecisionReason">>},
    {error, {"field \"required\" requires certain keys to exist", {[<<"reason">>, <<"yesno">>], Data4}}} = schema_validator:validate_data(?TESTSCHEMA1, Data4),

    Data5 = #{<<"reason">> => 25, <<"yesno">> => <<"yes">>},
    ResSchema5 = #{<<"description">> => <<"The reason for your decision">>,
        <<"title">> => <<"Reason">>,
        <<"type">> => <<"string">>},
    {error, {"field \"type\" requires the type integer or number", {ResSchema5, 25}}} = schema_validator:validate_data(?TESTSCHEMA1, Data5),

    ResSchema6 = ResSchema5,
    Data6 = #{<<"reason">> => true, <<"yesno">> => <<"yes">>},
    {error, {"field \"type\" requires the type boolean", {ResSchema6, true}}} = schema_validator:validate_data(?TESTSCHEMA1, Data6),

    ResSchema7 = ResSchema5,
    Data7 = #{<<"reason">> => [<<"DecisionReason">>], <<"yesno">> => <<"yes">>},
    {error, {"field \"type\" requires the type list", {ResSchema7, [<<"DecisionReason">>]}}} = schema_validator:validate_data(?TESTSCHEMA1, Data7),

    ResSchema8 = ResSchema5,
    Data8 = #{<<"reason">> => null, <<"yesno">> => <<"yes">>},
    {error, {"field \"type\" requires the type null", {ResSchema8, null}}} = schema_validator:validate_data(?TESTSCHEMA1, Data8),

    Data9 = #{<<"reason">> =>#{<<"properties">> => <<"DecisionReason">>}, <<"yesno">> => <<"yes">>},
    Value9 = #{<<"properties">> => <<"DecisionReason">>},
    ResSchema9 = ResSchema5,
    {error, {"field \"type\" requires the type object", {ResSchema9, Value9}}} = schema_validator:validate_data(?TESTSCHEMA1, Data9),

    Data10 = #{ <<"yesno">> => <<"yes">>},
    {ok, Data10} = schema_validator:validate_data(?TESTSCHEMA13, Data10).

validate_json_schema_testsuite(_Config) ->
    {ok, Files} = file:list_dir("./../../test/json-testsuite/"),
    lists:foreach(fun(File) -> exec_json_test(File) end, Files).    %otherwise badarith error

exec_json_test(FileName) ->
    Path = "./../../test/json-testsuite/",
    {ok, JsonData} = file:read_file(Path ++ FileName),
    Cases = jsx:decode(JsonData, [return_maps]),
    lists:foreach(fun(E) -> exec_json_case_test(maps:get(<<"schema">>, E), maps:get(<<"tests">>,E)) end, Cases).

%%    lists:foreach(fun(E) ->
%%        Schema = maps:get(<<"schema">>,E),
%%        Tests = maps:get(<<"tests">>, E),
%%        lists:foldl(fun(Test, Acc) -> {ok, _} = schema_validator:validate_data(Schema,maps:get(<<"data">>, Test)) end, {ok, any}, Tests)
%%        end, CasesArray).

exec_json_case_test(Schema, Tests) when is_list(Tests) ->
    lists:foreach(fun(E) -> exec_json_case_test(Schema, E) end,Tests);
exec_json_case_test(Schema, Test) when is_map(Test) ->
    ct_helper:print_var("Test", Test),
    case schema_validator:validate_data(Schema, maps:get(<<"data">>, Test)) of
        {ok, _} ->
            true = maps:get(<<"valid">>, Test);
        {error, Error} ->
            ct_helper:print_var("ValidatorError", Error),
            false = maps:get(<<"valid">>, Test)
    end.