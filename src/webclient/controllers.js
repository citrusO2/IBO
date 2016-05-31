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
            $scope.currentStep = null;
            $scope.groups = null;
            $scope.newStep = {};
            $scope.editStep = {};
            var iid = 0;    //internal running id to link steps

            // get all groups from the directory actor
            AuthService.getGroups(
                function(res){
                    $scope.groups = res.data;
                }, function(res){
                    $scope.error = "Could not retrieve usergroups! ";
                    if(res.status == 500)
                        $scope.error += "Internal server error (500)";
                    else if(res.status == 404)
                        $scope.error += "Cannot find the requested resource (404)";
                    else if(res.status == 403)
                        $scope.error += "You do not have access the requested resource (403)";
                }
            );

//            $scope.$watch('newStep.domain', function(){
//                if($scope.newStep.domain != null)
//                    $scope.newStep.localType = $scope.newStep.domain.info.local;
//            });

            //retrieve xactor information from the server
            $http.get('/api/repo/domain', AuthService.currentHeader()).then(
                function(res){
                    $scope.xactors = res.data;
                    $scope.newStep.domain = $scope.xactors[0];   // preselecting first item in select
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


            //initialise GUI
            var canvas = $('#modelCanvas');
            var defaultCanvasHeight = canvas.height();
            var graph = new joint.dia.Graph();
            var paper = new joint.dia.Paper({
                el: canvas,
                width: canvas.width(),
                height: canvas.height(),
                gridSize: 5,
                model: graph,
                restrictTranslate: true, // so that elements cannot move outside the bounding box
            });

            //initialise events on the GUI (need to use $scope.$apply because angular does not know about the changes otherwise)
            paper.on('cell:pointerclick', function(cellView, evt, x, y) {   // clicked on any cell
                //set clicked step active and possible last active step normal
                var Step = getStepFromCellView(cellView);
                if (Step != null){
                    if($scope.currentStep != null){
                        setStepNormal($scope.currentStep);
                    }
                    $scope.$apply(function(){
                        $scope.currentStep = Step;
                    });
                    setStepActive(Step);
                } else if($scope.currentStep != null) {
                    setStepNormal($scope.currentStep);
                    $scope.currentStep = null;
                }
            });
            paper.on('cell:pointerdblclick', function(cellView, evt, x, y) {   // clicked on any cell
                //set clicked step active and possible last active step normal
                var Step = getStepFromCellView(cellView);
                if (Step != null){
                    $scope.currentStep = Step;
                    $scope.$apply(function(){
                        $scope.currentStep = Step;
                        $scope.editCurrentStep();
                    });
                }
            });
            paper.on('blank:pointerclick', function(evt, x,y){  // clicked anywhere on the empty GUI
                //set the active step to normal
                if($scope.currentStep != null) {
                    setStepNormal($scope.currentStep);
                    $scope.currentStep = null;
                    $scope.$apply(function(){
                        $scope.currentStep = null;
                    });
                }
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

            $scope.createNewStep = function(Description, Domain, Local){
                var Step = {
                   description: Description,
                   domain: Domain,
                   local: Local,
                   commands: [],
                   umlState: getUmlState(Description, Domain, Local, []),
                   iid: (iid++).toString()
                };
                $scope.steps.push(Step);
                graph.addCell(Step.umlState);
                $scope.newStep.description = undefined;
                $scope.newStep.domain = $scope.xactors[0];
                $scope.newStep.local = null;
                $('#newStepModal').modal('hide');
            };

            $scope.setStartStep = function(Step){
                $scope.startStep = Step;
                if (startTransition == null){
                    startTransition = getUmlTransition(startState.id, Step.umlState.id, "start");
                    graph.addCell(startTransition);
                } else {
                    startTransition.set('target', {id: Step.umlState.id});
                }
            };

            //delete currently selected step
            $scope.deleteCurrentStep = function(){
                var r = confirm("Do you really want to delete the current active step?");
                if(r == true){
                    if(startTransition != null && startTransition.attributes.target.id == $scope.currentStep.umlState.id){
                        startTransition.remove();
                        startTransition = null;
                    }
                    $scope.steps.splice(getStepIndex($scope.currentStep), 1);
                    $scope.currentStep.umlState.remove();
                    $scope.currentStep = null;
                    console.log($scope.steps);
                }
            };

            //edit currently selected step
            $scope.editCurrentStep = function(){
                $scope.editStep = angular.extend({}, $scope.currentStep);  //using editStep instead of currentStep, so that the user can cancel/confirm changes
                if($scope.currentStep.commands == null){    // init commands
                    $scope.editStep.commands = [];
                } else {
                    $scope.editStep.commands = angular.copy($scope.currentStep.commands);
                };
                if($scope.editStep.initCommand == null && $scope.editStep.domain.info.init != null){   //init the init-command
                    $scope.editStep.initCommand = {
                        args: [],
                        command: 'init',
                        library: $scope.currentStep.domain.info.init,
                    };
                    console.log($scope.editStep.initCommand);
                } else if($scope.editStep.initCommand != null && $scope.editStep.domain.info.init != null){
                    $scope.editStep.initCommand = angular.copy($scope.currentStep.initCommand);
                }
                $('#editStepModal').modal('show');
//                console.log($scope.currentStep);
//                console.log($scope.editStep);
            };
            $scope.logEvent = function(info, event) {
                $scope.editStep.commands[event]

                console.log(info);
                console.log(event);
            };
            $scope.saveEditStep = function(Description, Domain, Local){
                $scope.currentStep.description = Description;
                $scope.currentStep.local = Local;
                //$scope.currentStep.commands = null;
                $scope.currentStep.commands = $scope.editStep.commands;
                $scope.currentStep.initCommand = $scope.editStep.initCommand;

                updateUmlState($scope.currentStep, Description, Domain, Local);
                $('#editStepModal').modal('hide');
            }

            $scope.editStepAddEmptyCommand = function(){
                if($scope.editStep.commands == null)
                    $scope.editStep.commands = [];
                $scope.editStep.commands.push({});
            }

            $scope.editStepRemoveCommand = function(Item){
                var index = $scope.editStep.commands.indexOf(Item);
                $scope.editStep.commands.splice(index, 1);
            }

            function getStepFromCellView(cellView){
                for (var i = 0; i < $scope.steps.length; i++) {
                    if($scope.steps[i].umlState.id == cellView.model.id)
                        return $scope.steps[i];
                }
            }

            function getStepIndex(step){
                for (var i = 0; i < $scope.steps.length; i++) {
                    if($scope.steps[i].umlState.id == step.umlState.id)
                        return i;
                }
            }

//            function getXactorByName(name){
//                for (var i = 0; i < $scope.xactors.length; i++) {
//                    if($scope.xactors[i].name == name)
//                        return $scope.xactors[i];
//                }
//            }

            function getUmlState(Description, Domain, Local, Commands){
                var umlName = getStateName(Description, Domain, Local);
                var umlWidth = umlName.length * 8 + 10;
                return new uml.State({
                    position: { x:100  , y: 100 },
                    size: { width: umlWidth, height: 100 },
                    name: umlName,
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

            function updateUmlState(Step, Description, Domain, Local, Commands){
                var umlName = getStateName(Description, Domain, Local);
                var umlWidth = umlName.length * 8 + 10;
                Step.umlState.set('name', umlName);
                //Step.umlState.set('size', { width: umlWidth, height: 100 });
                Step.umlState.resize(umlWidth, 100);
            }

            function getUmlTransition(IdSource, IdTarget, LabelText){
                var trans = new uml.Transition({
                    source: {id: IdSource},
                    target: {id: IdTarget},
                    attrs: {
                        '.connection' : {
                            'fill': 'none',
                            'stroke-linejoin': 'round',
                            'stroke-width': '2',
                            'stroke': '#4b4a67'
                        },
                        //'.marker-vertices': { display : 'none' },   // to disable the direct modification of the links
                        '.marker-arrowheads': { display: 'none' },
                        //'.connection-wrap': { display: 'none' },
                        '.link-tools': { display : 'none' }
                    }
                });
                trans.label(0, {
                    position: .5,
                    attrs: {
                      rect: { fill: '#f5f5f5' },
                      text: { fill: '#4b4a67', text: LabelText }
                    }
                });

                return trans;
            }

            function getStateName(Description, Domain, Local){
                Domain = (Domain == null ? {name : undefined } : Domain);
                //Local = (Local == null ? {name : undefined} : Local);
                if(Local == null ){
                    return Description + "@" + Domain.name;
                } else {
                    return Description + "@" + Domain.name + "\\" + Local.name ;
                }
            }

            function setStepActive(Step){
                Step.umlState.set('attrs', {
                    '.uml-state-body': {
                        fill: 'rgba(255, 255, 0, 0.1)',
                        stroke: 'rgba(255, 255, 0, 0.5)'
                    },
                    '.uml-state-separator': {
                        stroke: 'rgba(255, 255, 0, 0.4)'
                    }
                });
            }

            function setStepNormal(Step){
                Step.umlState.set('attrs', {
                    '.uml-state-body': {
                        fill: 'rgba(48, 208, 198, 0.1)',
                        stroke: 'rgba(48, 208, 198, 0.5)'
                    },
                    '.uml-state-separator': {
                        stroke: 'rgba(48, 208, 198, 0.4)'
                    }
                });
            }
        }
    ]);

})();