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

    iboControllers.controller('OverviewCtrl', ['$scope', 'AuthService',
        function ($scope, AuthService) {
            $scope.user = AuthService.currentUser();
        }
    ]);

    iboControllers.controller('BoxdetailCtrl', ['$scope', '$routeParams',
        function($scope, $routeParams) {
            $scope.xboId = $routeParams.xboId;
        }
    ]);

})();