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
                    $scope.error = "Could not retrieve args for the box! ";
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

    iboControllers.controller('NewProcCtrl', ['$scope', '$routeParams','$http','AuthService',
        function($scope, $routeParams, $http, AuthService) {
            $scope.error = null;
            $scope.processes = [];

            $http.get('/api/repo/process', AuthService.currentHeader()).then(
                function(res){
                    $scope.processes = res.data;
                },function(res){
                    $scope.error = "Could not retrieve processes for the box! ";
                    if(res.status == 500)
                        $scope.error += "Internal server error (500)";
                    else if(res.status == 404)
                        $scope.error += "Cannot find the requested resource (404)";
                    else if(res.status == 403)
                        $scope.error += "You do not have access the requested resource (403)";
                }
            );

            $scope.startprocess = function(processName){
                var r = confirm("Start process \""+processName+"\"?");
                if(r == true){
                    $http.post('/api/repo/process'+processName,processName,AuthService.currentHeader()).then(
                        function(res){$scope.success = "Process started successfully";},
                        function(res){$scope.error = res.data.error;}
                    );
                }
            };
        }
    ]);

    iboControllers.controller('NewProcTemplCtrl', ['$scope', '$routeParams','$http','AuthService',
        function($scope, $routeParams, $http, AuthService) {
            $scope.error = null;
            $scope.steps = [];
            $scope.startStep = null;
            $scope.xactors = null;

            $http.get('/api/repo/domain', AuthService.currentHeader()).then(
                function(res){
                    $scope.xactors = res.data;
                    //console.log(res.data);
                },function(res){
                    $scope.error = "Could not retrieve running xactors! ";
                    if(res.status == 500)
                        $scope.error += "Internal server error (500)";
                    else if(res.status == 404)
                        $scope.error += "Cannot find the requested resource (404)";
                    else if(res.status == 403)
                        $scope.error += "You do not have access the requested resource (403)";
                }
            );

            var canvas = $('#modelCanvas');
            var defaultCanvasHeight = canvas.height();
            var graph = new joint.dia.Graph();
            var paper = new joint.dia.Paper({
                el: canvas,
                width: canvas.width(),
                height: canvas.height(),
                gridSize: 1,
                model: graph,
                restrictTranslate: true, // so that elements cannot move outside the bounding box
            });
            var uml = joint.shapes.uml;
            var startState = new uml.StartState({
                position: { x:20  , y: 20 },
                size: { width: 30, height: 30 },
                attrs: {
                    'circle': {
                        fill: '#4b4a67',
                        stroke: 'none'
                    }
                }
            });
            var endState = new uml.EndState({
                position: { x:750  , y: 550 },
                size: { width: 30, height: 30 },
                attrs: {
                    '.outer': {
                        stroke: "#4b4a67",
                        'stroke-width': 2
                    },
                    '.inner': {
                        fill: '#4b4a67'
                    }
                }
            });
            var startTransition = null;
            graph.addCells([startState, endState]);

            $(window).resize(function() {
                paper.setDimensions(canvas.width(), defaultCanvasHeight);
            });

            $scope.newStep = function(Description, Domain, Local){
                var Step = {
                   description: Description,
                   domain: Domain,
                   local: Local,
                   commands: [],
                   umlState: getUmlState(Description, Domain, Local, [])
                };
                $scope.steps.push(Step);
                graph.addCell(Step.umlState);
                $('#newStepModal').modal('hide');
            };

            $scope.setStartStep = function(Step){
                $scope.startStep = Step;
                if (startTransition == null){
                    startTransition = getUmlTransition(startState.id, Step.umlState.id);
                    graph.addCell(startTransition);
                } else {
                    startTransition.set('target', {id: Step.umlState.id});
                }
            };

            function getUmlState(Description, Domain, Local, Commands){
                return new uml.State({
                    position: { x:100  , y: 100 },
                    size: { width: 200, height: 100 },
                    name: getStateName(Description, Domain, Local),
                    events: ["entry / init()","exit / destroy()"],
                    attrs: {
                        '.uml-state-body': {
                            fill: 'rgba(48, 208, 198, 0.1)',
                            stroke: 'rgba(48, 208, 198, 0.5)',
                            'stroke-width': 1.5
                        },
                        '.uml-state-separator': {
                            stroke: 'rgba(48, 208, 198, 0.4)'
                        }
                    }
                });
            }

            function getUmlTransition(IdSource, IdTarget){
                return new uml.Transition({
                    source: {id: IdSource},
                    target: {id: IdTarget},
                    attrs: {
                        '.connection' : {
                            'fill': 'none',
                            'stroke-linejoin': 'round',
                            'stroke-width': '2',
                            'stroke': '#4b4a67'
                        },
                        '.marker-vertices': { display : 'none' },   // to disable the direct modification of the links
                        '.marker-arrowheads': { display: 'none' },
                        '.connection-wrap': { display: 'none' },
                        '.link-tools': { display : 'none' }
                    }
                });
            }

            function getStateName(Description, Domain, Local){
                if(Local == ""){
                    return Description + "@" + Domain;
                } else {
                    return Description + "@" + Local + ":" + Domain;
                }
            }
        }
    ]);

})();