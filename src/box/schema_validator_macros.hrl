%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     based on https://github.com/klarna/jesse/blob/master/src/jesse_schema_validator.hrl
%%%     (Original work licensed under Apache License V2)
%%%     updating keywords to json schema draft4
%%% @end
%%% Created : 09. Dez 2015 23:31
%%%-------------------------------------------------------------------

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

%% http://json-schema.org/latest/json-schema-validation.html TODO: go through entire draft
%% VALIDATION NOTICE: Some validation keywords only apply to one or more primitive types.
%% When the primitive type of the instance cannot be validated by a given keyword,
%% validation for this keyword and instance SHOULD succeed.
%% Constant definitions for Json schema keywords (with tests in official suits)
-define(ADDITIONALITEMS,        <<"additionalItems">>).         % NOT IMPLEMENTED YET
-define(ADDITIONALPROPERTIES,   <<"additionalProperties">>).    % NOT IMPLEMENTED YET
-define(ALLOF,                  <<"allOf">>).                   % NOT IMPLEMENTED YET
-define(ANYOF,                  <<"anyOf">>).                   % NOT IMPLEMENTED YET
-define(DEFAULT,                <<"default">>).                 % NOT IMPLEMENTED YET
-define(DEFINITIONS,            <<"definitions">>).             % NOT IMPLEMENTED YET
-define(DEPENDENCIES,           <<"dependencies">>).            % NOT IMPLEMENTED YET
-define(ENUM,                   <<"enum">>).
-define(ITEMS,                  <<"items">>).
-define(MAXITEMS,               <<"maxItems">>).                % NOT IMPLEMENTED YET
-define(MAXLENGTH,              <<"maxLength">>).               % NOT IMPLEMENTED YET
-define(MAXPROPERTIES,          <<"maxProperties">>).           % NOT IMPLEMENTED YET
-define(MAXIMUM,                <<"maximum">>).                 % NOT IMPLEMENTED YET
-define(MINITEMS,               <<"minItems">>).                % NOT IMPLEMENTED YET
-define(MINLENGTH,              <<"minLength">>).               % NOT IMPLEMENTED YET
-define(MINPROPERTIES,          <<"minProperties">>).           % NOT IMPLEMENTED YET
-define(MINIMUM,                <<"minimum">>).                 % NOT IMPLEMENTED YET
-define(MULTIPLEOF,             <<"multipleOf">>).              % NOT IMPLEMENTED YET
-define(NOT,                    <<"not">>).                     % NOT IMPLEMENTED YET
-define(ONEOF,                  <<"oneOf">>).                   % NOT IMPLEMENTED YET
-define(PATTERN,                <<"pattern">>).                 % NOT IMPLEMENTED YET
-define(PATTERNPROPERTIES,      <<"patternProperties">>).       % NOT IMPLEMENTED YET
-define(PROPERTIES,             <<"properties">>).
-define(_REF,                   <<"$ref">>).                    % NOT IMPLEMENTED YET
-define(REQUIRED,               <<"required">>).



% additional definitions
-define(TYPE,                 <<"type">>).
-define(EXCLUSIVEMINIMUM,     <<"exclusiveMinimum">>).      % NOT IMPLEMENTED YET
-define(EXCLUSIVEMAXIMUM,     <<"exclusiveMaximum">>).      % NOT IMPLEMENTED YET
-define(UNIQUEITEMS,          <<"uniqueItems">>).           % NOT IMPLEMENTED YET
-define(FORMAT,               <<"format">>).                % NOT IMPLEMENTED YET



