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
                        //console.log(error);
                        if(error.status == 401){
                            $scope.error = "Could not authorize login, check username and password!";
                        } else{
                            $scope.error = "Could not authorize login, try again later!";
                            $scope.error += " (" + error.status + ": " + error.statusText + (error.data.error != null ? ", " + error.data.error +")" : ")");
                        }
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
            $scope.user = AuthService.currentUser();
            $scope.$watch( AuthService.isLoggedIn, function ( value ) {
                $scope.isLoggedIn = value;
                if(value){
                    $scope.user = AuthService.currentUser();
                }
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
            $scope.indices = DataService.currentIndices();
            $scope.error = null;

            $scope.$on('indicesChange', function(event, indices) {
                $scope.indices = indices
            });
            DataService.updateIndices();
        }
    ]);

    iboControllers.controller('BoxdetailCtrl', ['$scope', '$routeParams','$http','AuthService','SchemaV4Service', '$sce','$location','DataService',
        function($scope, $routeParams, $http, AuthService, SchemaV4Service, $sce, $location, DataService) {
            $scope.xboId = $routeParams.xboId;
            $scope.error = null;
            $scope.emptySchema = false;

            $http.get('/api/box/' + $scope.xboId /*, AuthService.currentHeader()*/).then(
                function(res){
                    $scope.schema = SchemaV4Service.orderSchema(res.data);
                    //update schema description to display variable templates better
                    $scope.schema.description = $sce.trustAsHtml(highlightVariables($scope.schema.description));

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
                            $http.post('/api/box/'+ $scope.xboId, $scope.model).then(onSendSuccess,onSendError);
                        }
                    }
                    $scope.emptySchema = Object.keys($scope.schema.properties).length == 0;

                    $scope.onEmptySubmit = function(){
                        $http.post('/api/box/'+ $scope.xboId, {} ).then(onSendSuccess,onSendError);
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

            function onSendSuccess(res){
                DataService.updateIndices();
                $scope.success = "Data sent successfully, redirecting...";
                fadeOutInputBox(function(){
                    setTimeout(function(){
                        $scope.$apply(function(){$location.path('overview');}); //needs $scope.$apply as $location.path is called by jquery and not angular
                    }, 2000);   //timeout so that the user can see what is happening
                });
            }

            function onSendError(res){
                $scope.error = res.data.error;
            }

            function fadeOutInputBox(FinishCallback){
                var $inputBox = $("#ibo-inputbox");
                $inputBox.width($inputBox.width()); //set the width absolute in pixel to avoid animation side effects
                $inputBox.animate({
                    marginLeft: parseInt($inputBox.css('marginLeft'),10) == 0 ? $inputBox.outerWidth() : 0
                }, 400, function(){
                    $inputBox.hide();
                    FinishCallback = FinishCallback || $.noop;
                    FinishCallback();
                });
            }

            function highlightVariables(DescriptionString){
                var str = DescriptionString;
                var matches = [];
                str.replace(/\[(.*?)\]/g, function(g0,g1){matches.push(g0);}); //g0 = including brackets, g1 = without brackets
                //var matches = DescriptionString.match(/\[(.*?)\]/g);
                for(var i = 0; i < matches.length; i++){
                    var split = matches[i].split('|');
                    var variable = (split[split.length-1]).slice(0, -1); //remove trailing brackets

                    var displayedVariable = '<code>' + variable + '</code>';
                    str =  str.split(matches[i]).join(displayedVariable); //instead of replace, replaces all occurrences //str.replace(matches[i], NewID + '|' + matches[1]);
                }
                return str.replace(/(?:\r\n|\r|\n)/g, '<br>');  //replace linebreaks with html breaks
            }
        }
    ]);

    iboControllers.controller('NewProcCtrl', ['$scope', '$routeParams','$http','AuthService', '$location',
        function($scope, $routeParams, $http, AuthService, $location) {
            $scope.error = null;
            $scope.processes = [];

            $http.get('/api/repo/process'/*, AuthService.currentHeader()*/).then(
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
                $http.post('/api/repo/process/'+processName,processName/*,AuthService.currentHeader()*/).then(
                    function(res){
                        $scope.success = "Process started successfully, redirecting...";
                        setTimeout(function(){
                            $scope.$apply(function(){$location.path('overview');});
                        }, 2000);   //timeout so that the user can see what is happening
                    },function(res){$scope.error = res.data.error;});
            };

            $scope.editprocess = function(processName){
                $scope.$apply(function(){
                    $location.path('template/' + processName);
                });
            };

            $scope.deletetemplate = function(processName, index){
                $http.delete('/api/repo/template/'+ processName).then(
                    function(res){
                        $scope.success = "Deleted template successfully";
                        $scope.processes.splice(index,1);
                    },
                    function(res){
                        if(res.status == 403)
                            $scope.error = "You do not have permission to delete the template"/*res.data.error;*/
                        else
                            $scope.error = "Error deleting template, error code " + res.status;
                    });
            };

        }
    ]);

    iboControllers.controller('ProcTemplCtrl', ['$scope', '$routeParams','$http','AuthService', 'SchemaV4Service',
        function($scope, $routeParams, $http, AuthService, SchemaV4Service) {
            $scope.isSaved = $routeParams.templateName == null ? false : true;
            $scope.error = null;
            $scope.steps = [];
            $scope.startStep = null;
            $scope.xactors = null;
            $scope.currentStep = null;
            $scope.groups = null;
            $scope.newStep = {};
            $scope.editStep = {};
            $scope.schema = createEmptySchema();
            $scope.condition = {};
            $scope.varselect = {};
            $scope.vars = {};
            $scope.templateSettings = {};
            var iid = 0;    //internal running id to link steps -> only used for GUI
            //var vid = 0;    //internal running id to link variables -> only used for GUI

            //retrieve xactor information from the server
            $http.get('/api/repo/domain' /*, AuthService.currentHeader()*/).then(
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
                restrictTranslate: true,    // so that elements cannot move outside the bounding box
                clickThreshold: 1           // fixes not being able to select a step
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

            // get all groups from the directory actor, load template if necessary as well
            AuthService.getGroups(
                function(res){
                    $scope.groups = res.data.filter(function(group){return (group.is_rulegroup == false);});    // only role groups
                    if($scope.isSaved){
                        loadTemplateFromServer();   //groups need to be loaded first, before trying to load the template
                    };
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

            // normal init done, check if template needs to be loaded from the repo-server
            function loadTemplateFromServer(){
                // console.log($routeParams.templateName);
                $http.get('/api/repo/template/' + $routeParams.templateName).then(
                    function(res){
                        //initialise GUI from template
                        var t = res.data;
                        //console.log(t);

                        //load basic information
                        $scope.templateSettings.name = t.name;
                        $scope.templateSettings.description = t.description;
                        //t.version skipped -> not in use
                        $scope.templateSettings.ttl = t.ttl;
                        $scope.templateSettings.groups = t.groups.map(function(groupname){ return getGroupByName(groupname);});

                        //add steps to internal steps, draw steps
                        for(var i = 0; i< t.steps.length; i++){
                            var is = t.steps[i];                    //is = imported step
                            var actor = getActorByName(is.domain);  // = actor-object
                            var s = {                               //new local step in memory from imported step
                                description: is.description,
                                domain: actor,
                                local: is.local,
                                commands: [],
                                iid: i+1,                            //at the initialisation, the internal step ids are equal to the step-index + 1 (= erlang id)
                                umlTransitions : []
                            }
                            s.umlState = createUmlState(s.description, s.domain, s.local, [], t.gui.steps[i].position.x, t.gui.steps[i].position.y);    //new empty uml-state at the right position but without the necessary transitions
                            //commands
                            for(var j = 0; j < t.steps[i].commands.length; j++){
                                if(actor.info.init != null && j == 0){
                                    s.initCommand = {
                                        library: is.commands[j].library,
                                        command: is.commands[j].command,
                                        args: getImportArgs(s, is.commands[j].library, is.commands[j].command, is.commands[j].args)
                                    };
                                }else{
                                    s.commands.push({
                                        library: is.commands[j].library,
                                        command: is.commands[j].command,
                                        args: getImportArgs(s, is.commands[j].library, is.commands[j].command, is.commands[j].args)
                                    });
                                }
                            }
                            $scope.steps.push(s);
                            graph.addCell(s.umlState);
                        }
                        //now update visual representation (= add transitions, change size depending on the commands)
                        for(var i = 0; i < $scope.steps.length; i++){
                            var s = $scope.steps[i];
                            var gsTransitions = t.gui.steps[i].transitions;
                            updateUmlState(s, s.description, s.domain, s.local);    //some updates in function updateUmlState are redundant, but it's not vital to change it atm.

                            //update vertices of the transitions (= markers users can set to determine the path/appearance of the transitions between the step)
                            var updatedTransitionIndices = [];
                            for(var y = 0; y < gsTransitions.length; y++){  //iterate over all transitions saved for the GUI (GuiSteps and normal Steps must match)
                                var transition = null;
                                if(gsTransitions[y].destination == "finish"){
                                    for(var z = 0; z < s.umlTransitions.length; z++){   //search for the right endtransition (there could be more than one!)
                                        if(s.umlTransitions[z].attributes.target.id == endState.id && updatedTransitionIndices.indexOf(z) == -1){
                                            //transition found!
                                            transition = s.umlTransitions[z];
                                            updatedTransitionIndices.push(z);
                                        }
                                    }
                                } else {
                                    var destUID = getStepByIID(gsTransitions[y].destination).umlState.id;   //when loading a template, the internal id of a state is equal to the erlang destination id
                                    for(var z = 0; z < s.umlTransitions.length; z++){
                                        if(s.umlTransitions[z].attributes.target.id == destUID && updatedTransitionIndices.indexOf(z) == -1){
                                            //transition found!
                                            transition = s.umlTransitions[z];
                                            updatedTransitionIndices.push(z);
                                        }
                                    }
                                }
                                transition.set('vertices', gsTransitions[y].vertices);
                            }
                        }
                        //update start and endpoint
                        startState.position(t.gui.start.point.x, t.gui.start.point.y);
                        $scope.setStartStep($scope.steps[(t.startstepnr-1)]);
                        startTransition.set('vertices', t.gui.start.vertices);
                        endState.position(t.gui.endpoint.x, t.gui.endpoint.y);

                        iid = t.steps.length + 1;   // set the internal step-id to a new empty internal id
                        updateStepVariables();      //steps are all initialised, create internal variable store
                    },function(res){
                        $scope.error = "Could not the template from the server! ";
                        if(res.status == 500)
                            $scope.error += "Internal server error (500)";
                        else if(res.status == 404)
                            $scope.error += "Cannot find the requested resource (404)";
                        else if(res.status == 403)
                            $scope.error += "You do not have access the requested resource (403)";
                    }
                );
            }

            // to prevent users from changing to a different page by accident
            $scope.$on('$locationChangeStart', function( event ) {
                var answer = confirm("Are you sure you want to leave this page?")
                if (!answer) {
                    event.preventDefault();
                }
            });

            $scope.createNewStep = function(Description, Domain, Local){
                var Step = {
                   description: Description,
                   domain: Domain,
                   local: Local,
                   commands: [],
                   umlState: createUmlState(Description, Domain, Local, []),
                   umlTransitions : [],
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
                    startTransition = createUmlTransition(startState.id, Step.umlState.id, "start");
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
                    //console.log($scope.steps);
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
                    //console.log($scope.editStep.initCommand);
                } else if($scope.editStep.initCommand != null && $scope.editStep.domain.info.init != null){
                    $scope.editStep.initCommand = angular.copy($scope.currentStep.initCommand);
                }
                $('#editStepModal').modal('show');
//                console.log($scope.currentStep);
//                console.log($scope.editStep);
            };
            $scope.editCurrentSchema = function(Args, Index){
                $scope.schema = angular.merge(createEmptySchema(), Args[Index]);   //initialise schema (=deep copy);
                $scope.schema.ref = { args: Args, index: Index };               //store reference to argument for saving later
                $('#schemaModal').modal('show');
            };
            $scope.editCurrentCondition = function(Args, argIndex){
                updateStepVariables();
                //console.log($scope.vars);
                $scope.condition = angular.merge({}, Args[argIndex]);       //initialise condition (=deep copy);
                $scope.condition.ref = { args: Args, index: argIndex };   //store reference to argument for saving later
                $('#conditionModal').modal('show');
            };
            $scope.insertNewVariableTemplate = function(Reference, ReferenceName){
                updateStepVariables();
                //console.log($scope.vars);
                $scope.varselect = {};
                $scope.varselect.ref = Reference;
                $scope.varselect.refName = ReferenceName;
                $('#varselectModal').modal('show');
            }
            $scope.insertSelectedVariableTemplate = function(varselect){
                varselect.ref[varselect.refName] += ' [' + varselect.step + "|" + varselect.variable + "]";
                $('#varselectModal').modal('hide');
            }

            $scope.previewCurrentSchema = function(Schema){
                var V4schema = SchemaV4Service.createSchema(Schema);
                $scope.schemaPreview = {
                    schema: V4schema,
                    form: [ "*", {type: "submit", title: "Send"}],
                    model: {},
                    onSubmit: function(form){
                        $scope.$broadcast('schemaFormValidate');
                        if(form.$valid){
                            window.alert("Form was successfully validated");
                        }
                    }
                };
                $('#schemaPreviewModal').modal('show');
            };
            $scope.saveCurrentSchema = function(){
                for(var i = 0; i < $scope.schema.variables.length; i++){
                    delete $scope.schema.variables[i].form; // delete reference to form -> otherwise angular crashes
//                    if($scope.schema.variables[i].vid == null)
//                        $scope.schema.variables[i].vid = vid++; //assign variable id if not yet assigned
                }
                $scope.schema.ref.args[$scope.schema.ref.index] = {     //saving schema on argument via reference
                    title: $scope.schema.title,
                    description: $scope.schema.description,
                    variables: $scope.schema.variables
                };
                $('#schemaModal').modal('hide');
            };
            $scope.saveCurrentCondition = function(){
                $scope.condition.ref.args[$scope.condition.ref.index] = {
                    step : $scope.condition.step,
                    variable: $scope.condition.variable,
                    operator: $scope.condition.operator,
                    value: $scope.condition.value
                };
                $('#conditionModal').modal('hide');
            }

            $scope.saveEditStep = function(Description, Domain, Local){
                $scope.currentStep.description = Description;
                $scope.currentStep.local = Local;
                //$scope.currentStep.commands = null;
                $scope.currentStep.commands = $scope.editStep.commands;
                $scope.currentStep.initCommand = $scope.editStep.initCommand;

                updateUmlState($scope.currentStep, Description, Domain, Local);
                $('#editStepModal').modal('hide');
            };

            $scope.editStepAddEmptyCommand = function(){
                if($scope.editStep.commands == null)
                    $scope.editStep.commands = [];
                $scope.editStep.commands.push({});
            }

            $scope.editStepRemoveCommand = function(Item){
                var index = $scope.editStep.commands.indexOf(Item);
                $scope.editStep.commands.splice(index, 1);
            };

            $scope.verifyDuplicateNames = function(){
                var sorted, i, isDuplicate;
                sorted = $scope.schema.variables.concat().sort(function (a, b) {
                    if (a.name > b.name) return 1;
                    if (a.name < b.name) return -1;
                    return 0;
                });
                for(i = 0; i < $scope.schema.variables.length; i++) {
                    isDuplicate = ((sorted[i-1] && sorted[i-1].name == sorted[i].name) || (sorted[i+1] && sorted[i+1].name == sorted[i].name));
                    sorted[i].form.varName.$setValidity('duplicate',!isDuplicate);
                }
            };

            $scope.openSaveTemplateDialog = function(){
                $('#templateSettingsModal').modal('show');
            }
            $scope.saveTemplate = function(){
                try{
                    var t = getExportTemplate();

                    $http.post('/api/repo/template/'+ t.name, t /*, AuthService.currentHeader()*/).then(
                        function(res){$scope.success = "Template stored successfully";},
                        function(res){$scope.error = res.data.error;}
                    );
                } catch(err){
                    $scope.error = "Error in template, saving not possible";
                }
                $('#templateSettingsModal').modal('hide');
            }

            function getStepByIID(iid){
                for(var i = 0; i < $scope.steps.length; i++){
                    if($scope.steps[i].iid == iid)
                        return $scope.steps[i];
                }
            }

            function getStepIndexByIID(iid){
                for(var i = 0; i < $scope.steps.length; i++){
                    if($scope.steps[i].iid == iid)
                        return i;
                }
                return -1;
            }

            function getStepNrByIID(iid){   // erlang lists start with index = 1
                return getStepIndexByIID(iid)+1;
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

            function getDestinationStepNrByTransition(transition){
                for(var i = 0; i < $scope.steps.length; i++){
                    if($scope.steps[i].umlState.id == transition.attributes.target.id)
                        return i+1;
                }
                if(transition.attributes.target.id == endState.id)
                    return "finish";

                throw "no destination identifier "
            }

//            function getXactorByName(name){
//                for (var i = 0; i < $scope.xactors.length; i++) {
//                    if($scope.xactors[i].name == name)
//                        return $scope.xactors[i];
//                }
//            }

            function createUmlState(Description, Domain, Local, Commands, X, Y){
                X = (X == null) ? 100 : X;
                Y = (Y == null) ? 100 : Y;
                var umlName = getStateName(Description, Domain, Local);
                var umlWidth = umlName.length * 8 + 10;

                return new uml.State({
                    position: { x: X  , y: Y },
                    size: { width: umlWidth, height: 40 },
                    name: umlName,
                    events: [],
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

            function getMaxLengthOfStrings(list){
                var length = list[0].length;
                for(var i = 1; i < list.length; i++){
                    if(length < list[i].length)
                        length = list[i].length;
                }
                if(arguments.length > 1){
                    for(var i = 1; i < arguments.length; i++)
                        if(length < arguments[i].length)
                            length = arguments[i].length;
                }
                return length;
            }

            function commandsToListOfStrings(commands, initCommand){
                var list = [];
                if(initCommand != null)
                    list.push(commandToString(initCommand));
                for(var i = 0; i < commands.length; i++)
                    list.push(commandToString(commands[i]));
                return list;
            }

            function commandToString(Command){
                var s = Command.library + "->" + Command.command;
                if(Command.args == null || Command.args.length == 0)
                    return s;
                return s + "->" + Command.args.join(",");
            }

            function updateUmlState(Step, Description, Domain, Local){
                //update text
                var umlName = getStateName(Description, Domain, Local);
                var commandList = commandsToListOfStrings(Step.commands, Step.initCommand);
                Step.umlState.set('name', umlName);
                Step.umlState.set('events', commandList);

                //update size
                var umlWidth = getMaxLengthOfStrings(commandList, umlName) * 8 + 10;
                var umlHeight = 30 + commandList.length * 14;
                //Step.umlState.set('size', { width: umlWidth, height: 100 });
                Step.umlState.resize(umlWidth, umlHeight);

                //update outgoing transitions (manual update and not simple delete/add to keep vertices of the transition)
                //1. detect changes, update/insert transitions accordingly
                var checked = []
                for(var i = 0; i < Step.commands.length; i++){
                    if(Step.commands[i].library == 'xlib_basic' && Step.commands[i].command == 'send' && Step.commands[i].args != null && Step.commands[i].args[0] != null){
                        var transLabel = "line" + (Step.initCommand == null ? i+1 : i+2) +":send";
                        //check if transition already exists
                        var oldTransIndex = getUmlSendTransitionIndexByCommand(Step.commands[i], Step.umlTransitions);
                        if(oldTransIndex != -1){

                            Step.umlTransitions[oldTransIndex].label(0, {attrs: { text: { text: transLabel}}}); //only try to update label
                            checked.push(oldTransIndex);
                        } else{
                            var destIID = Step.commands[i].args[0];
                            var destUID = getStepByIID(destIID).umlState.id;
                            var trans = createUmlTransition(Step.umlState.id, destUID, transLabel);
                            graph.addCell(trans);
                            checked.push(Step.umlTransitions.length);
                            Step.umlTransitions.push(trans);
                        }
                    }
                    if(Step.commands[i].library == 'xlib_basic' && Step.commands[i].command == 'finish'){
                        var transLabel = "line" + (Step.initCommand == null ? i+1 : i+2) +":finish";
                        var oldTransIndex = getUmlFinishTransitionIndex(Step.umlTransitions);
                        if(oldTransIndex != -1){
                            Step.umlTransitions[oldTransIndex].label(0, {attrs: { text: { text: transLabel}}});
                            checked.push(oldTransIndex);
                        }
                        else{
                            var trans = createUmlTransition(Step.umlState.id, endState.id, transLabel);
                            graph.addCell(trans);
                            checked.push(Step.umlTransitions.length);
                            Step.umlTransitions.push(trans);
                        }
                    }
                }
                //2. remove transitions which are not used any more
                for(var i = 0; i < Step.umlTransitions.length; i++)
                    if( checked.indexOf(i) == -1 ){
                        Step.umlTransitions[i].remove();
                        Step.umlTransitions.splice(i, 1);
                    }
            }

            function getUmlSendTransitionIndexByCommand(Command, umlTransitions){
                for(var i = 0; i < umlTransitions.length; i++){
                    var destIID = Command.args[0];                          // iid of the command's destination
                    var destUID = getStepByIID(destIID).umlState.id;        // umlState id of the destination
                    if( destUID == umlTransitions[i].attributes.target.id ) // return index if target's uml-id is the same as the transition's target's uml-id
                        return i;
                }
                return -1;
            }

            function getUmlFinishTransitionIndex(umlTransitions){
                for(var i = 0; i < umlTransitions.length; i++){
                    if( endState.id == umlTransitions[i].attributes.target.id )
                        return i;
                }
                return -1;
            }

            function createUmlTransition(IdSource, IdTarget, LabelText){
                var trans = new uml.Transition({
                    source: {id: IdSource},
                    target: {id: IdTarget},
                    //router: {name: 'orthogonal'},
                    connector: {name: 'rounded'},
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
                    return Description + "@" + Domain.name + "\\" + Local ;
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

            function createEmptySchema(){
                return angular.merge(getInitSchema(), {variables: []});
            }

            function getInitSchema(){
                return {
                    variablesceletons: [    // used to generate empty schema variables with the given default values
                        {
                            element: 'input',
                            required: true,
                            type: 'string'
                        },{
                            element: 'select',
                            required: true,
                            type: 'string'
                        }
                    ]
                };
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

            function updateStepVariables(){
                $scope.vars = {};
                for(var i = 0; i < $scope.steps.length; i++){
                    $scope.vars[ $scope.steps[i].iid ] = getVariablesFromStep($scope.steps[i]);
                }
            }

            function getVariablesFromStep(Step){
                //get all variables from schemas
                var vars = {};
                var libName, commandName;
                var commands = angular.copy(Step.commands);
                if(Step.initCommand != null)
                    commands.push(angular.copy(Step.initCommand));
                for(var i = 0; i < commands.length; i++){
                    libName = commands[i].library;
                    commandName = commands[i].command;
                    for(var y = 0; y < commands[i].args.length; y++){
                        if(isArgSchema(Step, libName, commandName, y)){
                            for(var j = 0; j < commands[i].args[y].variables.length; j++){
                                //vars[commands[i].args[y].variables[j].vid] = commands[i].args[y].variables[j];
                                vars[commands[i].args[y].variables[j].name] = commands[i].args[y].variables[j];
                            }
                            //vars = vars.concat(commands[i].args[y].variables);
                        }
                    }
                }
                return vars;
            }

            function isArgSchema(Step, libName, commandName, argIndex){
                return getArgType(Step, libName, commandName, argIndex) == 'schema';
            }

            function getArgType(Step, libName, commandName, argIndex){
                return Step.domain.info.libraries[libName][commandName].args[argIndex].type
            }

            function getExportTemplateGroupNames(){
                var g = [];
                for(var i = 0; i < $scope.templateSettings.groups.length; i++){
                    g.push($scope.templateSettings.groups[i].name);
                }
                return g;
            }

            function getExportTemplate(){
                return {
                    name: $scope.templateSettings.name,
                    description: $scope.templateSettings.description,
                    version: 1, // not used, gets set automatically on the serverside
                    ttl: $scope.templateSettings.ttl, // ttl in seconds
                    steps: getExportSteps(),
                    groups:  getExportTemplateGroupNames(),
                    startstepnr: getStepNrByIID($scope.startStep.iid),
                    startdestination: getStepByIID($scope.startStep.iid).domain.name,
                    gui: {
                        steps : getGuiSteps(),
                        start : { point : startState.attributes.position, vertices : startTransition.attributes.vertices},
                        endpoint : endState.attributes.position,
                    }   // umlState.attributes.position.x / .y
                };      // umlTransitions[index].attributes.vertices[index2].x / .y
            }           // -startState.attributes.position.x / .y
                        // -startSTate.attributes.vertices[index].x / .y
                        // -endState.attributes.position.x /.y

            function getGuiSteps(){
                var gs = [];    //guisteps
                for(var i = 0; i < $scope.steps.length; i++){
                    var v = [];
                    var transitions = $scope.steps[i].umlTransitions;
                    for(var j = 0; j < transitions.length; j++){
                        v.push({
                            destination: getDestinationStepNrByTransition(transitions[j]),
                            vertices: transitions[j].attributes.vertices
                        });
                    }

                    gs.push({
                        position: $scope.steps[i].umlState.attributes.position,
                        transitions: v
                    });
                }
                return gs;
            }

            function getExportSteps(){
                var s = angular.copy($scope.steps);
                var u = [];
                for(var i = 0; i < s.length; i++){
                    var coms = []
                    if(s[i].initCommand != null){   // if there is an initCommand, add it to the beginning of the other commands
                        s[i].commands.unshift(s[i].initCommand);
                    }

                    for(var j = 0; j < s[i].commands.length; j++){
                        var lib = s[i].commands[j].library;
                        var com = s[i].commands[j].command;
                        var args = s[i].commands[j].args;
                        coms.push({
                            library :   lib,
                            command :   com,
                            args :      getExportArgs(s[i],lib,com, args)
                        });
                    }

                    u.push({
                        domain :        s[i].domain.name,
                        local :         s[i].local,
                        description :   s[i].description,
                        commands :      coms
                    });
                }
                return u;
            }

            function getExportArgs(Step, Library, Command, Args){
                var a = [];
                for(var i = 0; i < Args.length; i++){
                    var type = getArgType(Step, Library, Command, i);
                    switch(type){
                        case "integer":
                            if(Library == "xlib_basic" && Command == "cjump")   //TODO: create own data type for command-nr, this is a hack and does not even consider init (+1 only if there is an init -> currently there is only the box which has an init, otherwise not!)
                                a.push(Args[i] + 1);
                            else
                                a.push(Args[i]);
                            break;
                        case "step": //redirect to index of step, instead of internal id
                            a.push(getStepNrByIID(Args[i]));    // step consists of first the new StepNr that should be executed and second of the destination name
                            a.push(Step.domain.name);
                            break;
                        case "condition":
                            var c = Args[i];
                            a.push([getStepNrByIID(c.step), c.variable, c.operator, c.value]);
                            //a.push([getStepNrByIID(c.step), $scope.vars[Step.iid][c.variable].name, c.operator, c.value]);  //redirect vid to variable name
                            break;
                        case "schema":
                            //console.log(Args[i]);
                            //update the internal IDs of the variabletemplates before creating the schema
                            Args[i].description = updateTemplateVariableIDs(Args[i].description);
                            a.push(SchemaV4Service.createSchema(Args[i]));
                            break;
                        default:
                            throw "error exporting template, an argument in one of the step's commands has an unsupported type";
                    }
                }
                return a;
            }

            //opposite transformation to getExportArgs(...)
            function getImportArgs(Step, Library, Command, Args){   //step variable needs to contain the domain as actor-object and
                var a = [];
                for(var i = 0; i < Args.length; i++){
                    var type = getArgType(Step, Library, Command, i);
                    switch(type){
                        case "integer":
                            if(Library == "xlib_basic" && Command == "cjump")   //TODO: create own data type for command-nr, this is a hack and does not even consider init (+1 only if there is an init -> currently there is only the box which has an init, otherwise not!)
                                a.push(Args[i] - 1);
                            else
                                a.push(Args[i]);
                            break;
                        case "step":
                            a.push(Args[i]);    // internal id equals the step-nr from erlang
                            i++;                // skipping the Step.domain.name, as it is redundant in the GUI
                            break;
                        case "condition":
                            var c = Args[i];
                            a.push({step: c[0], variable: c[1], operator: c[2], value: c[3]});
                            break;
                        case "schema":
                            a.push(SchemaV4Service.createInternalSchema(Args[i]));
                            break;
                        default:
                            throw "error exporting template, an argument in one of the step's commands has an unsupported type";
                    }
                }
                return a;
            }

            function getActorByName(Name){
                for(var i = 0; i < $scope.xactors.length; i++){
                    if(Name == $scope.xactors[i].name){
                        return $scope.xactors[i];
                    }
                }
                $scope.error = "Could not get the actor " + Name +", please try again later when the actor is available again";
                throw "Given actor not found!";
            }

            function getGroupByName(Name){
                for(var i = 0; i < $scope.groups.length; i++){
                    if(Name == $scope.groups[i].name){
                        return $scope.groups[i];
                    }
                }
                $scope.error = "Could not get the group " + Name +", please try again later when the group is available again";
                throw "Given group not found!";
            }

            function updateTemplateVariableIDs(DescriptionString){
                var str = DescriptionString;
                var matches = [];
                str.replace(/\[(.*?)\]/g, function(g0,g1){matches.push(g0);});
                //var matches = DescriptionString.match(/\[(.*?)\]/g);
                for(var i = 0; i < matches.length; i++){
                    var SplitVars = matches[i].split('|');
                    if(SplitVars.length == 2){
                        var OldID = SplitVars[0].substr(1); // remove first character, which is the '[' .slice(0, -1);
                        var NewID = getStepNrByIID(OldID);
                        if(NewID == 0)  // step cannot be found, so just skip the element
                            continue;
                        var NewTemplate = '[' + NewID + '|' + matches[i].split('|')[1];
                        str =  str.split(matches[i]).join(NewTemplate); //instead of replace, replaces all occurrences //str.replace(matches[i], NewID + '|' + matches[1]);
                    }
                }
                return str;
            }

        }
    ]);

})();