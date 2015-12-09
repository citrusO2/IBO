%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     TODO: Write own schema-validator, so that the form which is created on the webclient via the xlib_box->webinit->args can also be validated on the server side
%%% @end
%%% Created : 08. Dez 2015 17:53
%%%-------------------------------------------------------------------
-module(schema_validator).
-author("Florian").

-define(CT(ThrowReason, ProblemValue, Expression),
    case Expression of true -> true;_ -> throw({ThrowReason, ProblemValue}) end).

%% API
-export([validate_schema/1, validate_data/2, validate_schema_and_data/2]).

validate_schema(Schema) ->
    try validate_map(Schema) of
        true ->
            {ok, Schema}
    catch
        _:Error ->
            {error, Error}
    end.

validate_schema_and_data(Schema, Data) ->
    case validate_schema(Schema) of
        {ok, _} ->
            validate(Schema, Data);
        {error, Error} ->
            {error, Error}
    end.

validate_data(Schema, Data) ->
    try validate(Schema, Data) of
        true ->
            {ok, Data}
    catch
        _:Error ->
            {error, Error}
    end.

%% Validating Schema -------------------------------------------------
%CMap = current map = parent map of current element
validate_keyvalue(<<"title">>, Title, _CMap) ->
    ?CT("title has to be of type binary", Title, is_binary(Title));
validate_keyvalue(<<"description">>, Description, _CMap) ->
    ?CT("description has to be of type binary", Description, is_binary(Description));
validate_keyvalue(<<"type">>, Type, _CMap) ->
    ?CT("type has to be one of the following: object, string, number, boolean, array, null", Type,
        lists:member(Type, [<<"object">>, <<"string">>, <<"number">>, <<"boolean">>, <<"array">>, <<"null">>]));
validate_keyvalue(<<"required">>, List, CMap) ->
    ?CT("required has to be of type list", List, is_list(List)) andalso
        ?CT("required elements have to be of type binary", List, lists:all(fun(E) -> is_binary(E) end, List)) andalso
        ?CT("required elements must match the keys in properties", List, lists:all(fun(E) ->
            lists:member(E, List) end, maps:keys(maps:get(<<"properties">>, CMap))));    % required has to match the keys in properties
validate_keyvalue(<<"enum">>, List, _CMap) ->
    ?CT("enum has to be of type list", List, is_list(List)) andalso
        ?CT("enum elements have to be of type binary", List, lists:all(fun(E) -> is_binary(E) end, List));
validate_keyvalue(Key, Map, _CMap) when is_map(Map) ->
    validate_map(Map, Key).

validate_map(Map) when is_map(Map) ->
    ?CT("the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", Map,
        ((maps:get(<<"type">>, Map) =:= <<"object">> andalso maps:is_key(<<"properties">>, Map) andalso not maps:is_key(<<"items">>, Map)) orelse
            (maps:get(<<"type">>, Map) =:= <<"array">> andalso maps:is_key(<<"items">>, Map) andalso not maps:is_key(<<"properties">>, Map)) orelse
            (lists:member(maps:get(<<"type">>, Map), [<<"string">>, <<"number">>, <<"boolean">>, <<"null">>]) andalso not maps:is_key(<<"items">>, Map) andalso not maps:is_key(<<"properties">>, Map))))
        andalso maps:fold(fun(K, Val, AccIn) -> AccIn andalso validate_keyvalue(K, Val, Map) end, true, Map).

validate_map(Map, <<"properties">>) when is_map(Map) ->
    maps:fold(fun(K, Val, AccIn) ->
        AccIn andalso validate_keyvalue(K, Val, Map) end, true, Map); % TODO: more checks here, because property-map must contain more objects and not any reserved keywort, e.g. description or title
validate_map(Map, _Key) when is_map(Map) ->
    validate_map(Map).

%% Validate Data against the Schema ----------------------------------
validate(Schema, ValuesList) when is_list(ValuesList) ->
    case ?CT("the given value is a list, but the schema is not of type array", {Schema, ValuesList}, maps:get(<<"type">>, Schema) =:= <<"array">>) of
        true ->
            SubSchema = maps:get(<<"items">>, Schema),
            lists:foldl(fun(Value, Acc) -> Acc andalso validate(SubSchema, Value) end, true, ValuesList)
    end;
validate(Schema, ValuesMap) when is_map(ValuesMap) ->
    case ?CT("the given value is a map, but the schema is not of type object", {Schema, ValuesMap}, maps:get(<<"type">>, Schema) =:= <<"object">>) of
        true ->
            RequiredList = maps:get(<<"required">>, Schema),
            PropertiesMap = maps:get(<<"properties">>, Schema),

            case validate_required(RequiredList, ValuesMap) andalso validate_values_in_properties(PropertiesMap, ValuesMap) of
                true ->
                    maps:fold(fun(K, Val, AccIn) ->
                        AccIn andalso validate(maps:get(K, PropertiesMap), Val) end, true, ValuesMap);
                false ->
                    false
            end
    end;
validate(Schema, Value) when is_binary(Value) ->
    case ?CT("the given value is of type binary, but the schema is not of type string", {Schema, Value}, maps:get(<<"type">>, Schema) =:= <<"string">>) of
        true ->
            case maps:is_key(<<"enum">>, Schema) of
                true ->
                    ?CT("The given value differs from the values in enum", {Schema, Value}, lists:member(Value, maps:get(<<"enum">>, Schema)));
                false ->
                    true
            end
    end;
validate(Schema, Value) when is_integer(Value) ->
    ?CT("the given value is of type integer, but the schema is not of type integer", {Schema, Value}, maps:get(<<"type">>, Schema) =:= <<"integer">>);
validate(Schema, Value) when is_number(Value) ->
    ?CT("the given value is of type number, but the schema is not of type number", {Schema, Value}, maps:get(<<"type">>, Schema) =:= <<"number">>);
validate(Schema, Value) when is_boolean(Value) ->
    ?CT("the given value is of type boolean, but the schema is not of type boolean", {Schema, Value}, maps:get(<<"type">>, Schema) =:= <<"boolean">>);
validate(Schema, null) ->
    ?CT("the given value is of type null, but the schema is not of type null", {Schema, null}, maps:get(<<"type">>, Schema) =:= <<"null">>).

validate_required(RequiredList, ValuesMap) ->
    ValueKeys = maps:keys(ValuesMap),
    ?CT("the given value-map does not contain all required keys", {RequiredList, ValuesMap}, lists:all(fun(E) ->
        lists:member(E, ValueKeys) end, RequiredList)).

validate_values_in_properties(PropertiesMap, ValuesMap) ->
    ValueKeys = maps:keys(ValuesMap),
    PropertyKeys = maps:keys(PropertiesMap),
    ?CT("the given value-map contains more properties than defined in the properties-map", {PropertiesMap, ValuesMap}, lists:all(fun(E) ->
        lists:member(E, PropertyKeys) end, ValueKeys)).
