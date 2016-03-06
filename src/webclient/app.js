"use strict";
(function(){

    var iboApp =  angular.module('iboApp', ['ngRoute','iboControllers', 'schemaForm']);

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

                    $http.get('/api/directory', currentHeader).then(
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
                currentHeader: function() { return currentHeader; }
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

})();