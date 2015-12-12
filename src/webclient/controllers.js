"use strict";
(function(){

    var iboControllers = angular.module('iboControllers', []);

    iboControllers.controller('LoginCtrl', ['$scope','AuthService', '$location',
        function($scope, AuthService, $location) {
            clearMsgs();

            $scope.login = function(){
                AuthService.login($scope.username, $scope.password,
                    function(success){
                        $scope.success = "Login successful, redirecting...";    //displayed too shortly!
                        clearMsgs();
                        $location.path('overview');
                    }, function(error){
                        $scope.error = "Could not authorize login, check username and password";
                    }
                );
                clearMsgs();
            };

            function clearMsgs(){
                $scope.username = null;
                $scope.password = null;
                $scope.error = null;
                $scope.success  = null;
            }
        }
    ]);

    //main view controller to show/hide screen-elements
    iboControllers.controller('ViewCtrl', ['$scope','AuthService',
        function($scope, AuthService) {
            $scope.isLoggedIn = AuthService.isLoggedIn();
            $scope.$watch( AuthService.isLoggedIn, function ( value ) {
                $scope.isLoggedIn = value;
            });

            $scope.logout = AuthService.logout;
        }
    ]);

    //dummy-controller for login page
    iboControllers.controller('NoOpCtrl', ['$scope',
        function($scope) {
        }
    ]);

    iboControllers.controller('NavCtrl', ['$scope',
        function($scope) {
            $scope.tasks = [];

            $scope.$on('indicesChange', function(event, indices) {
                $scope.tasks = [];
                indices.forEach( function(index){
                    angular.forEach(index.xbolist, function(task, i){
                        $scope.tasks.push(task);
                    });
                });
            });
        }
    ]);

    iboControllers.controller('OverviewCtrl', ['$scope', 'AuthService', '$http', 'DataService',
        function ($scope, AuthService, $http, DataService) {
            $scope.user = AuthService.currentUser();
            $scope.indices = null;
            $scope.error = null;

            $http.get('/api/box', AuthService.currentHeader()).then(
                function(res){
                    $scope.indices = res.data;
                    DataService.broadcastindices($scope.indices)
//                    var nav = $('#activeTasks');
//                    nav.empty();
//                    $scope.indices.forEach( function(index){
//                        angular.forEach(index.xbolist, function(task, i){
//                            nav.append($compile("<li><a href=\"#/box/" + task.xboid + "\" data-active-link=\"active\">" + task.xbotemplate + "</a></li>") );
//                        });
//                    });
                },function(res){
                    $scope.error = "Could not retrieve box-indices!";
                }
            );
        }
    ]);

    iboControllers.controller('BoxdetailCtrl', ['$scope', '$routeParams','$http','AuthService',
        function($scope, $routeParams, $http, AuthService) {
            $scope.xboId = $routeParams.xboId;
            $scope.error = null;

            $http.get('/api/box/' + $scope.xboId, AuthService.currentHeader()).then(
                function(res){
                    $scope.schema = res.data

                    $scope.form = [
                        "*",
                        {
                          type: "submit",
                          title: "Send"
                        }
                    ];
                    $scope.model = {};

                    $scope.onSubmit = function(form) {
                        // First we broadcast an event so all fields validate themselves
                        $scope.error = null;
                        $scope.success = null;
                        $scope.$broadcast('schemaFormValidate');

                        // Then we check if the form is valid
                        if (form.$valid) {
                            console.log($scope.model);
                            $http.post('/api/box/'+ $scope.xboId, $scope.model, AuthService.currentHeader()).then(
                                function(res){$scope.success = "Data sent successfully";},
                                function(res){$scope.error = res.data.error;}
                            );
                        }
                    }

                },function(res){
                    $scope.error = "Could not retrieve args for the box!<br>";
                    if(res.status == 500)
                        $scope.error += "Internal server error (500)";
                    else if(res.status == 404)
                        $scope.error += "Cannot find the requested resource (404)";
                    else if(res.status == 403)
                        $scope.error += "You do not have access the requested resource (403)";
                }
            );
        }
    ]);

})();