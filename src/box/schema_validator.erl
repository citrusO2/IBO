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

%% API
-export([is_valid_schema/1, validate_data/2]).

is_valid_schema(Schema) ->
    validate_map(Schema).

validate_data(Schema, Data) ->
    throw(not_yet_implemented).

%% Validating Schema -------------------------------------------------
%CMap = current map = parent map of current element
validate_keyvalue(<<"title">>, Title, _CMap) ->
    is_binary(Title);
validate_keyvalue(<<"description">>, Description, _CMap) ->
    is_binary(Description);
validate_keyvalue(<<"type">>, Type, _CMap) ->
    lists:member(Type, [<<"object">>, <<"string">>, <<"number">>, <<"boolean">>, <<"array">>, <<"null">>]);
validate_keyvalue(<<"required">>, List, CMap) ->
    is_list(List) andalso
        lists:all(fun(E) -> is_binary(E) end, List) andalso
        lists:all(fun(E) -> lists:member(E, List) end, maps:keys(maps:get(<<"properties">>, CMap)));    % required has to match the keys in properties
validate_keyvalue(<<"enum">>, List, _CMap) ->
    is_list(List) andalso lists:all(fun(E) -> is_binary(E) end, List);
validate_keyvalue(Key, Map, _CMap) when is_map(Map) ->
    validate_map(Map, Key).

validate_map(Map) when is_map(Map) ->
    ((maps:get(<<"type">>,Map) =:= <<"object">> andalso maps:is_key(<<"properties">>,Map) andalso not maps:is_key(<<"items">>,Map)) orelse
    (maps:get(<<"type">>,Map) =:= <<"array">> andalso maps:is_key(<<"items">>,Map) andalso not maps:is_key(<<"properties">>,Map)) orelse
    (lists:member(maps:get(<<"type">>,Map),[<<"string">>, <<"number">>, <<"boolean">>,<<"null">>]) andalso not maps:is_key(<<"items">>,Map) andalso not maps:is_key(<<"properties">>,Map)))
        andalso maps:fold(fun(K, Val, AccIn) -> AccIn andalso validate_keyvalue(K, Val, Map) end, true, Map).

validate_map(Map, <<"properties">>) when is_map(Map) ->
    maps:fold(fun(K, Val, AccIn) -> AccIn andalso validate_keyvalue(K, Val, Map) end, true, Map);
validate_map(Map, _Key) when is_map(Map) ->
    validate_map(Map).
