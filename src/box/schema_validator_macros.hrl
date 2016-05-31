%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     defining keywords of json schema draft v4
%%% @end
%%% Created : 09. Dez 2015 23:31
%%%-------------------------------------------------------------------

%% Validation Macro Helper
-define(CT(ThrowReason, ProblemValue, Expression),
    case Expression of true -> true;_ -> throw({ThrowReason, ProblemValue}) end).

-define(MUSTBETYPE_TEXT, " must be of type ").
-define(ATLEASTONE_TEXT, " must have at least one element").
-define(UNIQUE_TEXT, "'s elements must all be unique").
-define(ALLOFTYPELIST_TEXT, "'s elements must all be of type list").
-define(ATLEASTZEROLENGTH_TEXT, " value must be greater or equal to zero").

-define(VALIDATE_FIELD_TYPE_TEXT, "field \"type\" requires the type ").
-define(VALIDATE_FIELD_REQUIRED_TEXT, "field \"required\" requires certain keys to exist").
-define(VALIDATE_FIELD_ENUM_TEXT, "field \"enum\" requires certain values").
-define(VALIDATE_FIELD_MINLENGTH_TEXT, "field \"minlength\" requires the string to have a certain length").
-define(VALIDATE_FIELD_MAXLENGTH_TEXT, "field \"maxlength\" requires the string to have a certain length").

-define(VALIDATE_FIELD_MAXIMUM_TEXT, "field \"maximum\" requires the number to have a certain maximum value").
-define(VALIDATE_FIELD_MINIMUM_TEXT, "field \"minimum\" requires the number to have a certain minimum value").

-define(ATLEASTONE(List, VariableName), ?CT(VariableName ++ ?ATLEASTONE_TEXT, List, erlang:length(List) >= 1)).
-define(UNIQUE(List, VariableName), ?CT(VariableName ++ ?UNIQUE_TEXT, List, erlang:length(List) == sets:size(sets:from_list(List)))).
-define(ALLOFTYPEBINARY(List, VariableName), ?CT(VariableName ++ ?ALLOFTYPELIST_TEXT, List, lists:all(fun(E) ->
    is_binary(E) end, List))).

-define(MUSTBETYPE(Var, VariableName, TypeName, Expression), ?CT(VariableName ++ ?MUSTBETYPE_TEXT ++ TypeName, Var, Expression)).
-define(MUSTBELIST(Var, VariableName), ?MUSTBETYPE(Var, VariableName, "list", is_list(Var))).
-define(MUSTBEBINARY(Var, VariableName), ?MUSTBETYPE(Var, VariableName, "binary", is_binary(Var))).
-define(MUSTBEINTEGER(Var, VariableName), ?MUSTBETYPE(Var, VariableName, "integer", is_integer(Var))).
-define(MUSTBENUMBER(Var, VariableName), ?MUSTBETYPE(Var, VariableName, "number", is_number(Var))).
-define(MUSTBEBOOLEAN(Var, VariableName), ?MUSTBETYPE(Var, VariableName, "boolean", is_boolean(Var))).
-define(MUSTBEPRIMITIVETYPE(Type), ?MUSTBETYPE(Type, "type", "primitive", lists:member(Type, [?OBJECT, ?STRING, ?NUMBER, ?BOOLEAN, ?ARRAY, ?NULL, ?INTEGER]))).

-define(ATLEASTZEROLENGTH(Var, VariableName), ?CT(VariableName ++ ?ATLEASTZEROLENGTH_TEXT, Var, Var >= 0)).

-define(VALIDATE_FIELD_TYPE(Schema, Variable, TypeName, TypeIdentifier), ?CT(?VALIDATE_FIELD_TYPE_TEXT ++ TypeName, {Schema, Variable}, not (maps:is_key(?TYPE, Schema)) orelse lists:member(TypeIdentifier, (case maps:get(?TYPE, Schema) of List when is_list(List) ->
    List; Else -> [Else] end)))).
-define(VALIDATE_FIELD_TYPE_OBJECT(Schema, Variable), ?VALIDATE_FIELD_TYPE(Schema, Variable, "object", ?OBJECT)).
-define(VALIDATE_FIELD_TYPE_NUMBER(Schema, Variable), ?VALIDATE_FIELD_TYPE(Schema, Variable, "number", ?NUMBER)).
-define(VALIDATE_FIELD_TYPE_BOOLEAN(Schema, Variable), ?VALIDATE_FIELD_TYPE(Schema, Variable, "boolean", ?BOOLEAN)).
-define(VALIDATE_FIELD_TYPE_BINARY(Schema, Variable), ?VALIDATE_FIELD_TYPE(Schema, Variable, "binary", ?STRING)).
-define(VALIDATE_FIELD_TYPE_NULL(Schema, Variable), ?VALIDATE_FIELD_TYPE(Schema, Variable, "null", ?NULL)).
-define(VALIDATE_FIELD_TYPE_LIST(Schema, Variable), ?VALIDATE_FIELD_TYPE(Schema, Variable, "list", ?ARRAY)).
-define(VALIDATE_FIELD_TYPE_INTEGER_OR_NUMBER(Schema, Variable), ?CT(?VALIDATE_FIELD_TYPE_TEXT ++ "integer or number", {Schema, Variable}, not (maps:is_key(?TYPE, Schema)) orelse
    lists:any(fun(E) ->
        lists:member(E, [?INTEGER, ?NUMBER]) end, (case maps:get(?TYPE, Schema) of List when is_list(List) ->
        List; Else -> [Else] end)
    ))).

-define(VALIDATE_FIELD_ENUM(Schema, Variable), ?CT(?VALIDATE_FIELD_ENUM_TEXT, {maps:get(?ENUM, Schema), Variable}, not (maps:is_key(?ENUM, Schema)) orelse lists:member(Variable, maps:get(?ENUM, Schema)))).

-define(VALIDATE_FIELD_REQUIRED(Schema, Variable), ?CT(?VALIDATE_FIELD_REQUIRED_TEXT, {maps:get(?REQUIRED, Schema), Variable}, not (maps:is_key(?REQUIRED, Schema)) orelse lists:all(fun(E) ->
    lists:member(E, maps:keys(Variable)) end, maps:get(?REQUIRED, Schema)))).

-define(VALIDATE_FIELD_MINLENGTH(Schema, Variable), ?CT(?VALIDATE_FIELD_MINLENGTH_TEXT, {maps:get(?MINLENGTH, Schema), Variable}, not (maps:is_key(?MINLENGTH, Schema)) orelse (
     maps:get(?MINLENGTH, Schema) =< erlang:length(binary:bin_to_list(Variable))
))).
-define(VALIDATE_FIELD_MAXLENGTH(Schema, Variable), ?CT(?VALIDATE_FIELD_MAXLENGTH_TEXT, {maps:get(?MAXLENGTH, Schema), Variable}, not (maps:is_key(?MAXLENGTH, Schema)) orelse (
    maps:get(?MAXLENGTH, Schema) >= erlang:length(binary:bin_to_list(Variable))
))).
-define(VALIDATE_FIELD_MAXIMUM(Schema, Variable), ? CT(?VALIDATE_FIELD_MAXIMUM_TEXT, {maps:get(?MAXIMUM, Schema), Variable}, not (maps:is_key(?MAXIMUM, Schema)) orelse (
    (not (maps:is_key(?EXCLUSIVEMAXIMUM, Schema)) andalso maps:get(?MAXIMUM, Schema) >= Variable) orelse
    ((maps:is_key(?EXCLUSIVEMAXIMUM, Schema)) andalso maps:get(?MAXIMUM, Schema) > Variable)
))).
-define(VALIDATE_FIELD_MINIMUM(Schema, Variable), ? CT(?VALIDATE_FIELD_MINIMUM_TEXT, {maps:get(?MINIMUM, Schema), Variable}, not (maps:is_key(?MINIMUM, Schema)) orelse (
    (not (maps:is_key(?EXCLUSIVEMINIMUM, Schema)) andalso maps:get(?MINIMUM, Schema) =< Variable) orelse
    ((maps:is_key(?EXCLUSIVEMINIMUM, Schema)) andalso maps:get(?MINIMUM, Schema) < Variable)
))).

-define(VALIDATE_MATCHING_PROPERTIES(Schema, Variable),
    ?CT("the given value-map contains more properties than defined in the properties-map", {maps:get(?PROPERTIES, Schema), ValuesMap}, not (maps:is_key(?PROPERTIES, Schema)) orelse lists:all(fun(E) ->
        lists:member(E, maps:keys(maps:get(?PROPERTIES, Schema))) end, maps:keys(ValuesMap)))).

%% http://json-schema.org/latest/json-schema-core.html

%% 3.5. JSON Schema primitive types
-define(ARRAY,      <<"array">>).
-define(BOOLEAN,    <<"boolean">>).
-define(INTEGER,    <<"integer">>).
-define(NULL,       <<"null">>).
-define(NUMBER,     <<"number">>).
-define(OBJECT,     <<"object">>).
-define(STRING,     <<"string">>).

%% 6. The "$schema" keyword
-define(_SCHEMA,    <<"$schema">>).                                 % NOT IMPLEMENTED YET
-define(DRAFT4REF,  <<"http://json-schema.org/draft-04/schema#">>). % NOT IMPLEMENTED YET

%% 7. URI resolution scopes and dereferencing
-define(ID, <<"id">>).  % NOT IMPLEMENTED YET

% additional definitions
-define(_REF,   <<"$ref">>).    % NOT IMPLEMENTED YET

%% http://json-schema.org/latest/json-schema-validation.html TODO: go through entire draft
%% VALIDATION NOTICE: Some validation keywords only apply to one or more primitive types.
%% When the primitive type of the instance cannot be validated by a given keyword,
%% validation for this keyword and instance SHOULD succeed.

%% 5.  Validation keywords sorted by instance types
%% 5.1.  Validation keywords for numeric instances (number and integer)
%% 5.1.1.1.  Valid values
%%      The value of "multipleOf" MUST be a JSON number. This number MUST be strictly greater than 0.
%% 5.1.1.2.  Conditions for successful validation
%%      A numeric instance is valid against "multipleOf" if the result of the division of the instance by this keyword's value is an integer.
-define(MULTIPLEOF, <<"multipleOf">>).  % NOT IMPLEMENTED YET

%% 5.1.2.  maximum and exclusiveMaximum
%% 5.1.2.1.  Valid values
%%      The value of "maximum" MUST be a JSON number. The value of "exclusiveMaximum" MUST be a boolean.
%%      If "exclusiveMaximum" is present, "maximum" MUST also be present.
%% 5.1.2.2.  Conditions for successful validation
%%      Successful validation depends on the presence and value of "exclusiveMaximum":
%%      if "exclusiveMaximum" is not present, or has boolean value false, then the instance is valid if it is lower than, or equal to, the value of "maximum";
%%      if "exclusiveMaximum" has boolean value true, the instance is valid if it is strictly lower than the value of "maximum".
%% 5.1.2.3.  Default value
%%      "exclusiveMaximum", if absent, may be considered as being present with boolean value false.
-define(MAXIMUM,            <<"maximum">>).
-define(EXCLUSIVEMAXIMUM,   <<"exclusiveMaximum">>).

%% 5.1.3.  minimum and exclusiveMinimum
%% 5.1.3.1.  Valid values
%%      The value of "minimum" MUST be a JSON number. The value of "exclusiveMinimum" MUST be a boolean.
%%      If "exclusiveMinimum" is present, "minimum" MUST also be present.
%% 5.1.3.2.  Conditions for successful validation
%%      Successful validation depends on the presence and value of "exclusiveMinimum":
%%      if "exclusiveMinimum" is not present, or has boolean value false, then the instance is valid if it is greater than, or equal to, the value of "minimum";
%%      if "exclusiveMinimum" is present and has boolean value true, the instance is valid if it is strictly greater than the value of "minimum".
%% 5.1.3.3.  Default value
%%      "exclusiveMinimum", if absent, may be considered as being present with boolean value false.
-define(MINIMUM,            <<"minimum">>).
-define(EXCLUSIVEMINIMUM,   <<"exclusiveMinimum">>).

%% 5.2.  Validation keywords for strings
%% 5.2.1.  maxLength
%% 5.2.1.1.  Valid values
%%      The value of this keyword MUST be an integer. This integer MUST be greater than, or equal to, 0.
%% 5.2.1.2.  Conditions for successful validation
%%      A string instance is valid against this keyword if its length is less than, or equal to, the value of this keyword.
%%      The length of a string instance is defined as the number of its characters as defined by RFC 4627 [RFC4627].
-define(MAXLENGTH,  <<"maxLength">>).

%% 5.2.2.  minLength
%% 5.2.2.1.  Valid values
%%      The value of this keyword MUST be an integer. This integer MUST be greater than, or equal to, 0.
%% 5.2.2.2.  Conditions for successful validation
%%      A string instance is valid against this keyword if its length is greater than, or equal to, the value of this keyword.
%%      The length of a string instance is defined as the number of its characters as defined by RFC 4627 [RFC4627].
%% 5.2.2.3.  Default value
%%      "minLength", if absent, may be considered as being present with integer value 0.
-define(MINLENGTH,  <<"minLength">>).

%% 5.2.3.  pattern
%% 5.2.3.1.  Valid values
%%      The value of this keyword MUST be a string. This string SHOULD be a valid regular expression, according to the ECMA 262 regular expression dialect.
%% 5.2.3.2.  Conditions for successful validation
%%      A string instance is considered valid if the regular expression matches the instance successfully. Recall: regular expressions are not implicitly anchored.
-define(PATTERN,    <<"pattern">>). % NOT IMPLEMENTED YET

%% 5.3.  Validation keywords for arrays
%% 5.3.1.  additionalItems and items
%% 5.3.1.1.  Valid values
%%      The value of "additionalItems" MUST be either a boolean or an object. If it is an object, this object MUST be a valid JSON Schema.
%%      The value of "items" MUST be either an object or an array. If it is an object, this object MUST be a valid JSON Schema. If it is an array, items of this array MUST be objects, and each of these objects MUST be a valid JSON Schema.
%% 5.3.1.2.  Conditions for successful validation
%%      Successful validation of an array instance with regards to these two keywords is determined as follows:
%%      if "items" is not present, or its value is an object, validation of the instance always succeeds, regardless of the value of "additionalItems";
%%      if the value of "additionalItems" is boolean value true or an object, validation of the instance always succeeds;
%%      if the value of "additionalItems" is boolean value false and the value of "items" is an array, the instance is valid if its size is less than, or equal to, the size of "items".
%% 5.3.1.3.  Example
%%      The following example covers the case where "additionalItems" has boolean value false and "items" is an array, since this is the only situation under which an instance may fail to validate successfully.
%%      This is an example schema:
%%      {
%%          "items": [ {}, {}, {} ],
%%          "additionalItems": false
%%      }
%%      With this schema, the following instances are valid:
%%          [] (an empty array),
%%          [ [ 1, 2, 3, 4 ], [ 5, 6, 7, 8 ] ],
%%          [ 1, 2, 3 ];
%%      the following instances are invalid:
%%          [ 1, 2, 3, 4 ],
%%          [ null, { "a": "b" }, true, 31.000002020013 ]
%% 5.3.1.4.  Default values
%%      If either keyword is absent, it may be considered present with an empty schema.
-define(ITEMS,              <<"items">>).           % NOT IMPLEMENTED YET
-define(ADDITIONALITEMS,    <<"additionalItems">>). % NOT IMPLEMENTED YET

%% 5.3.2.  maxItems
%% 5.3.2.1.  Valid values
%%      The value of this keyword MUST be an integer. This integer MUST be greater than, or equal to, 0.
%% 5.3.2.2.  Conditions for successful validation
%%      An array instance is valid against "maxItems" if its size is less than, or equal to, the value of this keyword.
-define(MAXITEMS,   <<"maxItems">>).    % NOT IMPLEMENTED YET

%% 5.3.3.  minItems
%% 5.3.3.1.  Valid values
%%      The value of this keyword MUST be an integer. This integer MUST be greater than, or equal to, 0.
%% 5.3.3.2.  Conditions for successful validation
%%      An array instance is valid against "minItems" if its size is greater than, or equal to, the value of this keyword.
%% 5.3.3.3.  Default value
%%      If this keyword is not present, it may be considered present with a value of 0.
-define(MINITEMS,   <<"minItems">>).    % NOT IMPLEMENTED YET

%% 5.3.4.  uniqueItems
%% 5.3.4.1.  Valid values
%%      The value of this keyword MUST be a boolean.
%% 5.3.4.2.  Conditions for successful validation
%%      If this keyword has boolean value false, the instance validates successfully. If it has boolean value true, the instance validates successfully if all of its elements are unique.
%% 5.3.4.3.  Default value
%%      If not present, this keyword may be considered present with boolean value false.
-define(UNIQUEITEMS,    <<"uniqueItems">>). % NOT IMPLEMENTED YET

%% 5.4.  Validation keywords for objects
%% 5.4.1.  maxProperties
%% 5.4.1.1.  Valid values
%%      The value of this keyword MUST be an integer. This integer MUST be greater than, or equal to, 0.
%% 5.4.1.2.  Conditions for successful validation
%%      An object instance is valid against "maxProperties" if its number of properties is less than, or equal to, the value of this keyword.
-define(MAXPROPERTIES,  <<"maxProperties">>).   % NOT IMPLEMENTED YET

%% 5.4.2.  minProperties
%% 5.4.2.1.  Valid values
%%      The value of this keyword MUST be an integer. This integer MUST be greater than, or equal to, 0.
%% 5.4.2.2.  Conditions for successful validation
%%      An object instance is valid against "minProperties" if its number of properties is greater than, or equal to, the value of this keyword.
%% 5.4.2.3.  Default value
%%      If this keyword is not present, it may be considered present with a value of 0.
-define(MINPROPERTIES,  <<"minProperties">>).   % NOT IMPLEMENTED YET

%% 5.4.3.  required
%% 5.4.3.1.  Valid values
%%      The value of this keyword MUST be an array. This array MUST have at least one element. Elements of this array MUST be strings, and MUST be unique.
%% 5.4.3.2.  Conditions for successful validation
%%      An object instance is valid against this keyword if its property set contains all elements in this keyword's array value.
-define(REQUIRED,   <<"required">>).

%% 5.4.4.  additionalProperties, properties and patternProperties
%% 5.4.4.1.  Valid values
%%      The value of "additionalProperties" MUST be a boolean or an object. If it is an object, it MUST also be a valid JSON Schema.
%%      The value of "properties" MUST be an object. Each value of this object MUST be an object, and each object MUST be a valid JSON Schema.
%%      The value of "patternProperties" MUST be an object. Each property name of this object SHOULD be a valid regular expression, according to the ECMA 262 regular expression dialect. Each property value of this object MUST be an object, and each object MUST be a valid JSON Schema.
%% 5.4.4.2.  Conditions for successful validation
%%      Successful validation of an object instance against these three keywords depends on the value of "additionalProperties":
%%      if its value is boolean true or a schema, validation succeeds;
%%      if its value is boolean false, the algorithm to determine validation success is described below.
%% 5.4.4.3.  Default values
%%      If either "properties" or "patternProperties" are absent, they can be considered present with an empty object as a value.
%%      If "additionalProperties" is absent, it may be considered present with an empty schema as a value.
%% 5.4.4.4.  If "additionalProperties" has boolean value false
%%      In this case, validation of the instance depends on the property set of "properties" and "patternProperties". In this section, the property names of "patternProperties" will be called regexes for convenience.
%%      The first step is to collect the following sets:
%%          s
%%              The property set of the instance to validate.
%%          p
%%              The property set from "properties".
%%          pp
%%              The property set from "patternProperties".
%%      Having collected these three sets, the process is as follows:
%%          remove from "s" all elements of "p", if any;
%%          for each regex in "pp", remove all elements of "s" which this regex matches.
%%      Validation of the instance succeeds if, after these two steps, set "s" is empty.
%%5.4.4.5.  Example
%%      This schema will be used as an example:
%%      {
%%          "properties": {
%%              "p1": {}
%%          },
%%          "patternProperties": {
%%              "p": {},
%%              "[0-9]": {}
%%          }
%%      }
%%      This is the instance to validate:
%%      {
%%          "p1": true,
%%          "p2": null,
%%          "a32&o": "foobar",
%%          "": [],
%%          "fiddle": 42,
%%          "apple": "pie"
%%      }
%%      The three property sets are:
%%          s
%%              [ "p1", "p2", "a32&o", "", "fiddle", "apple" ]
%%          p
%%              [ "p1" ]
%%          pp
%%              [ "p", "[0-9]" ]
%%      Applying the two steps of the algorithm:
%%          after the first step, "p1" is removed from "s";
%%          after the second step, "p2" (matched by "p"), "a32&o" (matched by "[0-9]") and "apple" (matched by "p") are removed from "s".
%%      The set "s" still contains two elements, "" and "fiddle". Validation therefore fails.
-define(ADDITIONALPROPERTIES,   <<"additionalProperties">>).    % NOT IMPLEMENTED YET
-define(PROPERTIES,             <<"properties">>).              % somewhat implemented
-define(PATTERNPROPERTIES,      <<"patternProperties">>).       % NOT IMPLEMENTED YET

%% 5.4.5.  dependencies
%% 5.4.5.1.  Valid values
%%      This keyword's value MUST be an object. Each value of this object MUST be either an object or an array.
%%      If the value is an object, it MUST be a valid JSON Schema. This is called a schema dependency.
%%      If the value is an array, it MUST have at least one element. Each element MUST be a string, and elements in the array MUST be unique. This is called a property dependency.
%% 5.4.5.2.  Conditions for successful validation
%% 5.4.5.2.1.  Schema dependencies
%%      For all (name, schema) pair of schema dependencies, if the instance has a property by this name, then it must also validate successfully against the schema.
%%      Note that this is the instance itself which must validate successfully, not the value associated with the property name.
%% 5.4.5.2.2.  Property dependencies
%%      For each (name, propertyset) pair of property dependencies, if the instance has a property by this name, then it must also have properties with the same names as propertyset.
-define(DEPENDENCIES,   <<"dependencies">>).    % NOT IMPLEMENTED YET

%% 5.5.  Validation keywords for any instance type
%% 5.5.1.  enum
%% 5.5.1.1.  Valid values
%%      The value of this keyword MUST be an array. This array MUST have at least one element. Elements in the array MUST be unique.
%%      Elements in the array MAY be of any type, including null.
%% 5.5.1.2.  Conditions for successful validation
%%      An instance validates successfully against this keyword if its value is equal to one of the elements in this keyword's array value.
-define(ENUM,   <<"enum">>).

%% 5.5.2.  type
%% 5.5.2.1.  Valid values
%%      The value of this keyword MUST be either a string or an array. If it is an array, elements of the array MUST be strings and MUST be unique.
%%      String values MUST be one of the seven primitive types defined by the core specification.
%% 5.5.2.2.  Conditions for successful validation
%%      An instance matches successfully if its primitive type is one of the types defined by keyword. Recall: "number" includes "integer".
-define(TYPE,   <<"type">>).

%% 5.5.3.  allOf
%% 5.5.3.1.  Valid values
%%      This keyword's value MUST be an array. This array MUST have at least one element.
%%      Elements of the array MUST be objects. Each object MUST be a valid JSON Schema.
%% 5.5.3.2.  Conditions for successful validation
%%      An instance validates successfully against this keyword if it validates successfully against all schemas defined by this keyword's value.
-define(ALLOF,  <<"allOf">>).   % NOT IMPLEMENTED YET

%% 5.5.4.  anyOf
%% 5.5.4.1.  Valid values
%%      This keyword's value MUST be an array. This array MUST have at least one element.
%%      Elements of the array MUST be objects. Each object MUST be a valid JSON Schema.
%% 5.5.4.2.  Conditions for successful validation
%%      An instance validates successfully against this keyword if it validates successfully against at least one schema defined by this keyword's value.
-define(ANYOF,  <<"anyOf">>).   % NOT IMPLEMENTED YET

%% 5.5.5.  oneOf
%% 5.5.5.1.  Valid values
%%      This keyword's value MUST be an array. This array MUST have at least one element.
%%      Elements of the array MUST be objects. Each object MUST be a valid JSON Schema.
%% 5.5.5.2.  Conditions for successful validation
%%      An instance validates successfully against this keyword if it validates successfully against exactly one schema defined by this keyword's value.
-define(ONEOF,  <<"oneOf">>).   % NOT IMPLEMENTED YET

%% 5.5.6.  not
%% 5.5.6.1.  Valid values
%%      This keyword's value MUST be an object. This object MUST be a valid JSON Schema.
%% 5.5.6.2.  Conditions for successful validation
%%      An instance is valid against this keyword if it fails to validate successfully against the schema defined by this keyword.
-define(NOT,    <<"not">>). % NOT IMPLEMENTED YET

%% 5.5.7.  definitions
%% 5.5.7.1.  Valid values
%%      This keyword's value MUST be an object. Each member value of this object MUST be a valid JSON Schema.
%% 5.5.7.2.  Conditions for successful validation
%%      This keyword plays no role in validation per se. Its role is to provide a standardized location for schema authors to inline JSON Schemas into a more general schema.
%%      As an example, here is a schema describing an array of positive integers, where the positive integer constraint is a subschema in "definitions":
%%      {
%%          "type": "array",
%%          "items": { "$ref": "#/definitions/positiveInteger" },
%%          "definitions": {
%%              "positiveInteger": {
%%                  "type": "integer",
%%                  "minimum": 0,
%%                  "exclusiveMinimum": true
%%              }
%%          }
%%      }
-define(DEFINITIONS,    <<"definitions">>). % NOT IMPLEMENTED YET

%% 6.  Metadata keywords
%% 6.1.  "title" and "description"
%% 6.1.1.  Valid values
%%      The value of both of these keywords MUST be a string.
%% 6.1.2.  Purpose
%%      Both of these keywords can be used to decorate a user interface with information about the data produced by this user interface. A title will preferrably be short, whereas a description will provide explanation about the purpose of the instance described by this schema.
%%      Both of these keywords MAY be used in root schemas, and in any subschemas.
-define(TITLE,          <<"title">>).
-define(DESCRIPTION,    <<"description">>).

%% 6.2.  "default"
%% 6.2.1.  Valid values
%%      There are no restrictions placed on the value of this keyword.
%% 6.2.2.  Purpose
%%      This keyword can be used to supply a default JSON value associated with a particular schema. It is RECOMMENDED that a default value be valid against the associated schema.
%%      This keyword MAY be used in root schemas, and in any subschemas.
-define(DEFAULT,    <<"default">>). % NOT IMPLEMENTED YET


%% ALSO SEE chapter 7 and forward!
-define(FORMAT, <<"format">>).  % NOT IMPLEMENTED YET



