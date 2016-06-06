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
            }).when('/newtemplate', {
                templateUrl: 'partials/proctemplate.html',
                controller: 'NewProcTemplCtrl',
                activenav: 'detail'
            }).otherwise({
                redirectTo: '/login'
            });
        }
    ]);

    //http://stackoverflow.com/questions/14206492/how-do-i-store-a-current-user-context-in-angular
    iboApp.factory('AuthService', ['$http','$location',
        function($http, $location) {
            var currentUser = null;
            var currentHeader = null;
            var currentGroups = null;

            function createHeader(username, password){
                currentHeader = {headers:  {
                    'Authorization': 'Basic ' + window.btoa(username + ":" + password),
                    'Accept': 'application/json',
                }};
            }

            //$location.path('login');

            return {
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
                },
                isLoggedIn: function() { return currentUser !== null; },
                currentUser: function() { return currentUser; },
                currentHeader: function() { return currentHeader; },
                currentGroups: function() { return currentGroups; },
                getGroups: function(success, error){
                    $http.get('/api/directory/group', this.currentHeader()).then(
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

    iboApp.factory('DataService', ['$rootScope',
        function($rootScope){
            return{
                broadcastindices: function(indices){
                    $rootScope.$broadcast('indicesChange', indices)
                }
            }
        }
    ])

    iboApp.run(['$rootScope', '$location', 'AuthService', function ($rootScope, $location, AuthService) {
        $rootScope.$on('$routeChangeStart', function (event, next, data2) {
            if (!AuthService.isLoggedIn()) {
                if(next.$$route && next.$$route.originalPath != '/login'){
                    event.preventDefault();
                    $location.path('/login');
                }
            }
            else {
                if(next.$$route && next.$$route.originalPath == '/login'){
                    event.preventDefault();
                    $location.path('/overview');
                }
            }
        });
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

    iboApp.factory('SchemaV4Service', [ function() {
        //creates the actual json schema used later by client+server
        var createSchemaV4 = function(Schema){
            return {
                title: Schema.title,
                description: Schema.description,
                type: 'object',
                properties: createSchemaV4properties(Schema.variables),
                required: getRequiredVariableNames(Schema.variables)
            }
        }

        function createSchemaV4properties(variables){
            var properties = {};
            for(var i = 0; i < variables.length; i++){
                properties[variables[i].name] = createSchemaV4variableProperties(variables[i])
            }
            return properties;
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


        return{
            createSchema: createSchemaV4
        }
    }]);

})();