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

-include("schema_validator_macros.hrl").

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

%%%===================================================================
%%% Validating Schema
%%%===================================================================
%CMap = current map = parent map of current element
validate_keyvalue(?TITLE, Title, _CMap) ->
    ?MUSTBEBINARY(Title, "title");
validate_keyvalue(?DESCRIPTION, Description, _CMap) ->
    ?MUSTBEBINARY(Description, "description");
validate_keyvalue(?TYPE, Type, _CMap) ->
    ?MUSTBEPRIMITIVETYPE(Type);
validate_keyvalue(?REQUIRED, List, CMap) ->
    ?MUSTBELIST(List,"required") andalso ?ATLEASTONE(List, "required") andalso
        ?ALLOFTYPEBINARY(List, "required") andalso ?UNIQUE(List, "required") andalso
        ?CT("required elements must match the keys in properties", List, lists:all(fun(E) ->
            lists:member(E, List) end, maps:keys(maps:get(?PROPERTIES, CMap))));    % required has to match the keys in properties (NOT IN THE DRAFT!!)
validate_keyvalue(?ENUM, List, _CMap) ->
    ?MUSTBELIST(List, "enum") andalso ?ATLEASTONE(List, "enum") andalso ?UNIQUE(List, "enum");
validate_keyvalue(Key, Map, _CMap) when is_map(Map) ->
    validate_map(Map, Key).

validate_map(Map) when is_map(Map) ->
    ?CT("the given map must be either of (type=object, properties, no items), (type=aray, items, no properties) or (type string/number/boolean/null, no items, no properties", Map,
        ((maps:get(?TYPE, Map) =:= ?OBJECT andalso maps:is_key(?PROPERTIES, Map) andalso not maps:is_key(?ITEMS, Map)) orelse
            (maps:get(?TYPE, Map) =:= ?ARRAY andalso maps:is_key(?ITEMS, Map) andalso not maps:is_key(?PROPERTIES, Map)) orelse
            (lists:member(maps:get(?TYPE, Map), [?STRING, ?NUMBER, ?BOOLEAN, ?NULL]) andalso not maps:is_key(?ITEMS, Map) andalso not maps:is_key(?PROPERTIES, Map))))
        andalso maps:fold(fun(K, Val, AccIn) -> AccIn andalso validate_keyvalue(K, Val, Map) end, true, Map).

validate_map(Map, ?PROPERTIES) when is_map(Map) ->
    maps:fold(fun(K, Val, AccIn) ->
        AccIn andalso validate_keyvalue(K, Val, Map) end, true, Map); % TODO: more checks here, because property-map must contain more objects and not any reserved keywort, e.g. description or title
validate_map(Map, _Key) when is_map(Map) ->
    validate_map(Map).

%%%===================================================================
%%% Validate Data against the Schema
%%%===================================================================
validate(Schema, ValuesList) when is_list(ValuesList) ->
    ?VALIDATE_FIELD_TYPE_LIST(Schema, ValuesList) andalso
    ?VALIDATE_FIELD_ENUM(Schema, ValuesList),

    case maps:find(?ITEMS, Schema) of
        {ok, _} ->
            SubSchema = maps:get(?ITEMS, Schema),
            lists:foldl(fun(Value, Acc) -> Acc andalso validate(SubSchema, Value) end, true, ValuesList);
        _ ->
            true
    end;
validate(Schema, ValuesMap) when is_map(ValuesMap) ->
    ?VALIDATE_FIELD_TYPE_OBJECT(Schema, ValuesMap) andalso
    ?VALIDATE_FIELD_REQUIRED(Schema, ValuesMap) andalso
    ?VALIDATE_FIELD_ENUM(Schema, ValuesMap) andalso
    case maps:size(ValuesMap) >= 1 of
        true ->
            ?VALIDATE_MATCHING_PROPERTIES(Schema, ValuesMap),
            PropertiesMap = maps:get(?PROPERTIES, Schema),
            maps:fold(fun(K, Val, AccIn) ->
                AccIn andalso validate(maps:get(K, PropertiesMap), Val) end, true, ValuesMap);
        false ->
            true
    end;
validate(Schema, Value) when is_binary(Value) ->
    ?VALIDATE_FIELD_TYPE_BINARY(Schema, Value),
    ?VALIDATE_FIELD_ENUM(Schema, Value);
validate(Schema, Value) when is_integer(Value) ->
    ?VALIDATE_FIELD_TYPE_INTEGER_OR_NUMBER(Schema, Value),
    ?VALIDATE_FIELD_ENUM(Schema, Value);
validate(Schema, Value) when is_number(Value) ->
    ?VALIDATE_FIELD_TYPE_NUMBER(Schema, Value),
    ?VALIDATE_FIELD_ENUM(Schema, Value);
validate(Schema, Value) when is_boolean(Value) ->
    ?VALIDATE_FIELD_TYPE_BOOLEAN(Schema, Value),
    ?VALIDATE_FIELD_ENUM(Schema, Value);
validate(Schema, null) ->
    ?VALIDATE_FIELD_TYPE_NULL(Schema, null),
    ?VALIDATE_FIELD_ENUM(Schema, null).
