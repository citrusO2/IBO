"use strict";
(function(){

    var iboApp =  angular.module('iboApp', ['ngRoute','iboControllers', 'schemaForm', 'dndLists', 'ngTagsInput']);

    iboApp.config(['$routeProvider',
        function($routeProvider) {
            $routeProvider.when('/login', {
                templateUrl: 'partials/dummy.html',
                controller: 'NoOpCtrl',
                activenav: 'none'
            }).when('/overview', {
                templateUrl: 'partials/overview.html',
                controller: 'OverviewCtrl',
                activenav: 'overview'
            }).when('/box/:xboId', {
                templateUrl: 'partials/boxdetail.html',
                controller: 'BoxdetailCtrl',
                activenav: 'detail'
            }).when('/proclist', {
                templateUrl: 'partials/proclist.html',
                controller: 'NewProcCtrl',
                activenav: 'detail'
            }).when('/template/:templateName', {
                templateUrl: 'partials/proctemplate.html',
                controller: 'ProcTemplCtrl',
                activenav: 'detail'
            }).when('/template', {
                templateUrl: 'partials/proctemplate.html',
                controller: 'ProcTemplCtrl',
                activenav: 'detail'
            }).otherwise({
                redirectTo: '/overview'
            });
        }
    ]);

    //http://stackoverflow.com/questions/14206492/how-do-i-store-a-current-user-context-in-angular
    iboApp.factory('AuthService', ['$http','$location','DataService',
        function($http, $location, DataService) {
            var currentUser = null;
            var currentHeader = null;
            var currentGroups = null;
            var wasCookieLoginTried = false;

            function createHeader(username, password){
                currentHeader = {headers:  {
                    'Authorization': 'Basic ' + window.btoa(username + ":" + password),
                    'Accept': 'application/json',
                }};
            }

            //$location.path('login');

            return {
                cookie_login: function(success, error){
                    wasCookieLoginTried = true;
                    $http.get('/api/directory/user').then(
                        function(res){
                            DataService.updateIndices();    //logged in successfully
                            if($location.path() == "/login"){
                                $location.path('overview');
                            }
                            currentUser = res.data;
                            success = success || $.noop;
                            success(res);
                        },function(res){
                            error = error || $.noop;
                            error(res);
                        }
                    );
                },
                login: function(username, password, success, error) {
                    createHeader(username, password);

                    $http.get('/api/directory/user', currentHeader).then(
                        function(res){
                            currentUser = res.data;
                            success = success || $.noop;
                            success(res);
                        },function(res){
                            error = error || $.noop;
                            error(res);
                        }
                    );
                },
                logout: function() {
                    currentUser = currentHeader = null;
                    $location.path('login');
                    $http.get('/api/directory/logout').then(    // deletes session-id from server to force authentication
                        function(res){
                            //console.log(res);   //logout successful
                        },function(res){
                            //console.log(res);   //logout incomplete
                        }
                    )
                },
                wasCookieLoginTried: function() { return wasCookieLoginTried},

                isLoggedIn: function() { return currentUser !== null; },
                currentUser: function() { return currentUser; },
                currentHeader: function() { return currentHeader; },
                currentGroups: function() { return currentGroups; },
                getGroups: function(success, error){
                    $http.get('/api/directory/group' /*, this.currentHeader()*/).then(
                        function(res){
                            currentGroups = res.data;
                            success = success || $.noop;
                            success(res);
                        }, function(res){
                            error = error || $.noop;
                            error(res);
                        }
                    )
                }
            };
        }
    ]);

    // redirect to login when 401 is received
    iboApp.service('authInterceptor',['$q', '$location',
        function($q, $location) {
            var service = this;

            service.responseError = function(response) {
                if (response.status == 401){
                    $location.path('/login');
                }
                return $q.reject(response);
            };
        }
    ]);
    iboApp.config(['$httpProvider', function($httpProvider) {
        $httpProvider.interceptors.push('authInterceptor');
    }])

    iboApp.factory('DataService', ['$rootScope','$http',
        function($rootScope, $http){
            var indices = [];
            var updating = false;

            function broadcastIndices(indices){
                $rootScope.$broadcast('indicesChange', indices)
            }
            function updateIndices(){
                if(!updating){
                    updating = true;
                    $http.get('/api/box').then(
                        function(res){
                            var indices = res.data.map(function(item){
                                item.xbolist = item.xbolist.map(function(element){
                                    element.storedate = new Date(element.storedate)
                                    return element;
                                });
                                return item;
                            });
                            updating = false;
                            broadcastIndices(indices);
                        }
                    ),function(res){
                        updating = false;
                        //todo: send/display error
                    }
                }
            }

            return{
                currentIndices: function(){return indices;},
                updateIndices: function(){updateIndices();}
            }
        }
    ])

    iboApp.run(['$rootScope', '$location', 'AuthService', function ($rootScope, $location, AuthService) {
        $rootScope.$on('$routeChangeStart', function (event, next, data2) {
            if (!AuthService.wasCookieLoginTried()){
                AuthService.cookie_login();
            } else if (!AuthService.isLoggedIn()) {
                if(next.$$route && next.$$route.originalPath != '/login'){
                    event.preventDefault();
                    $location.path('/login');
                }
            }
            else {
                if(next.$$route && next.$$route.originalPath == '/login'){
                    event.preventDefault();
                    AuthService.logout();
                    //$location.path('/overview');
                }
            }
        });
    }]);

    iboApp.directive( 'goClick', [ '$location', function ($location) {
        return function ( scope, element, attrs ) {
            var path;
            attrs.$observe( 'goClick', function (val) {
                path = val;
            });
            element.bind( 'click', function () {
                scope.$apply( function () {
                    $location.path( path );
                });
            });
        };
    }]);

    iboApp.directive('confirmClick', [ function(){
        return {
            link: function (scope, element, attr) {
                var msg = attr.confirmClick || "Are you sure?";
                var clickAction = attr.confirmedClick;
                element.bind('click',function (event) {
                    if ( window.confirm(msg) ) {
                        scope.$eval(clickAction)
                    }
                });
            }
        };
    }]);

    iboApp.directive('activeLink', ['$location', function (location) {
        return {
            restrict: 'A',
            link: function(scope, element, attrs, controller) {
                var clazz = attrs.activeLink;
                var path = attrs.href;
                path = path.substring(1); //hack because path does not return including hashbang
                scope.location = location;
                scope.$watch('location.path()', function (newPath) {
                    if (path === newPath) {
                        element.parent().addClass(clazz);
                    } else {
                        element.parent().removeClass(clazz);
                    }
                });
            }
        };
    }]);

    //workaround to prevent users from using init functions in the list of commands by simply filtering
    iboApp.filter('noInitProperty', [ function(){
        return function (object){
            var Keys = {};
            for(var keyName in object){
                if(keyName != 'init')
                    Keys[keyName] = 'dummy';    //only the keyname is relevant, the value is ignored
            }
            return Keys;
        };
    }]);

    //http://stackoverflow.com/questions/11160513/angularjs-ng-options-create-range
    iboApp.filter('range', function() {
        return function(input, min, max, skip) {
            min = parseInt(min); //Make string input int
            max = parseInt(max);
            skip = parseInt(skip)
            //console.log("skip", skip, "min", min, "max", max, "input", input);
            for (var i=min; i<max; i++){
                if(i != skip)
                    input.push(i);
            }
            return input;
        };
    });

    iboApp.factory('SchemaV4Service', [ function() {
        //creates the actual json schema used later by client+server
        var createSchemaV4 = function(Schema){
            var Schema = {
                title: Schema.title,
                description: Schema.description,
                type: 'object',
                properties: createSchemaV4properties(Schema.variables),
                required: getRequiredVariableNames(Schema.variables),
                order: Schema.variables.map(function(variable){return variable.name}) // not official schema input, but needed because erlang does not retain order in maps
            }
            if(Schema.required.length == 0)
                delete Schema.required;
            if(Schema.order.length == 0)
                delete Schema.order;

            return Schema;
        }

        var createInternalSchema = function(SchemaV4){
            return {
                title: SchemaV4.title,
                description: SchemaV4.description,
                variables: createInternalSchemaVariables(orderSchemaV4(SchemaV4))
            };
        }

        function createSchemaV4properties(variables){
            var properties = {};
            for(var i = 0; i < variables.length; i++){
                properties[variables[i].name] = createSchemaV4variableProperties(variables[i])
            }
            return properties;
        }

        function createInternalSchemaVariables(SchemaV4){
            var variables = [];
            angular.forEach(SchemaV4.properties, function(props, varname){
                var newvar = {  // general properties of the variable
                    name: varname,
                    title: props.title,
                    description: props.description,
                    required: isVariableRequired(SchemaV4.required,varname),
                    type: props.type
                };  // order is ignored, because it has already been taken care of before

                if(props.enum != null){ //input type select
                    newvar.element = "select";
                    newvar.options =  props.enum.map(function(variable){return {text: variable}});
                } else {    //input type input
                    newvar.element = "input"
                    switch(props.type){
                        case "integer": //no break
                        case "number":
                            newvar.hasMinimum = props.minimum != null;
                            newvar.hasMaximum = props.maximum != null;
                            if(newvar.hasMinimum)
                                newvar.minimum = props.minimum;
                            if(newvar.hasMaximum)
                                newvar.maximum = props.maximum;
                            break;
                        case "string":
                            newvar.hasMinLength = props.minLength != null;
                            newvar.hasMaxLength = props.maxLength != null;
                            if(newvar.hasMinLength)
                                newvar.minLength = props.minLength;
                            if(newvar.hasMaxLength)
                                newvar.maxLength = props.maxLength;
                            break;
                        default:
                            throw "element type input is not of type integer, number or string!"
                    }
                }
                variables.push(newvar);
            });
            return variables;
        }

        function isVariableRequired(RequiredVariables, VariableName){
            if(RequiredVariables == null)
                return false;

            for(var i = 0; i < RequiredVariables.length; i++){
                if(RequiredVariables[i] == VariableName)
                    return true;
            }
            return false;
        }

        function createSchemaV4variableProperties(variable){
            var prop = {
                title: variable.title,
                description: variable.description,
                type: variable.type
            };

            if(variable.element == 'input'){
                if(variable.type == 'number' || variable.type == 'integer'){
                    if(variable.hasMinimum)
                        prop.minimum = variable.minimum;
                    if(variable.hasMaximum)
                        prop.maximum = variable.maximum;
                } else if(variable.type == 'string'){
                    if(variable.hasMinLength)
                        prop.minLength = variable.minLength;
                    if(variable.hasMaxLength)
                        prop.maxLength = variable.maxLength;
                } else {
                     throw "update createSchemaV4variableProperties, input-element of type " + variable.type + " is not yet defined";
                }
            } else if(variable.element == 'select'){
                if(variable.type == 'string'){
                    prop.enum = getEnumValues(variable.options);
                } else {
                    throw "update createSchemaV4variableProperties, select-element of type " + variable.type + " is not yet defined";
                }
            } else {
                throw "update createSchemaV4variableProperties, element " + variable.element + " is not yet defined";
            }
            return prop;
        }

        function getEnumValues(Options){
            var e = [];
            for(var i = 0; i < Options.length; i++){
                e.push(Options[i].text);
            }
            return e;
        }

        function getRequiredVariableNames(Variables){
            var required = [];
            for(var i = 0; i < Variables.length; i++){
                if(Variables[i].required)
                    required.push(Variables[i].name);
            }
            return required;
        }

        var orderSchemaV4 = function(Schema){
            if(Schema.order == null)
                return Schema;

            var orderedProperties = {};
            for(var i = 0; i < Schema.order.length; i++){
                orderedProperties[Schema.order[i]] = Schema.properties[Schema.order[i]];    // reordering works, but keep in mind that variable inspecting usually displays properties in alphabetic order
            }
            Schema.properties = orderedProperties;
            return Schema;
        }

        return{
            createSchema: createSchemaV4,
            orderSchema: orderSchemaV4,
            createInternalSchema: createInternalSchema
        };
    }]);

})();