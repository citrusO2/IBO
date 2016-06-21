%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%     Helper to gracefully prepare records for conversion to json
%%% @end
%%% Created : 06. Dez 2015 22:51
%%%-------------------------------------------------------------------
-module(json_helper).
-author("Florian").

-include("handler_macros.hrl").
-include("../box/box_records.hrl").
-include("../repo/repo_records.hrl").
-include("../xbo/xbo_records.hrl").

%% API ---------------------------------------------------------------
-export([prepare/1, prepareXactors/1, prepareTemplate/2, checkTemplate/1]).

prepare(List) when is_list(List) ->
    lists:map(fun(Element)-> prepare(Element) end, List);
prepare(Template) when is_record(Template, ibo_repo_template) ->
    PreparedTemplate = Template#ibo_repo_template{
        steps = prepare(Template#ibo_repo_template.steps),
        gui = prepare(Template#ibo_repo_template.gui),
        transform = undefined   % transform should not be transmitted to web-client, should one be available
    },
    ?record_to_map(ibo_repo_template, PreparedTemplate);
prepare(Commmand) when is_record(Commmand, ibo_xboline) ->
    ?record_to_map(ibo_xboline, Commmand);
prepare(Step) when is_record(Step, ibo_xbostep) ->
    PreparedStep = Step#ibo_xbostep{commands = prepare(Step#ibo_xbostep.commands)},
    ?record_to_map(ibo_xbostep, PreparedStep);
prepare(Gui) when is_record(Gui, ibo_repo_gui) ->
    PreparedGui = Gui#ibo_repo_gui{
        endpoint = preparePoint(Gui#ibo_repo_gui.endpoint),
        start = #{<<"point">> => preparePoint(element(1, Gui#ibo_repo_gui.start)),<<"vertices">> => preparePoint(element(2, Gui#ibo_repo_gui.start)) },
        steps = prepare(Gui#ibo_repo_gui.steps)
    },
    ?record_to_map(ibo_repo_gui, PreparedGui);
prepare(GuiStep) when is_record(GuiStep, ibo_repo_gui_step) ->
    PreparedGuiStep = GuiStep#ibo_repo_gui_step{
        position = preparePoint(GuiStep#ibo_repo_gui_step.position),
        transitions = prepare(GuiStep#ibo_repo_gui_step.transitions)
    },
    ?record_to_map(ibo_repo_gui_step, PreparedGuiStep);
prepare(Transition) when is_record(Transition, ibo_repo_gui_transition) ->
    PreparedTransition = Transition#ibo_repo_gui_transition{
        vertices = preparePoint(Transition#ibo_repo_gui_transition.vertices)
    },
    ?record_to_map(ibo_repo_gui_transition, PreparedTransition);
prepare(Ref) when is_record(Ref, ibo_boxindex) ->
    SubR = [?record_to_tuplelist(ibo_boxindex_elementpreview,
        E#ibo_boxindex_elementpreview{
            storedate = calendar:now_to_local_time(E#ibo_boxindex_elementpreview.storedate)
        }) || E <- Ref#ibo_boxindex.xbolist ],
    R1 = Ref#ibo_boxindex{xbolist = SubR},
    ?record_to_tuplelist(ibo_boxindex, R1);
prepare(Element) ->
    Element.

preparePoint(ListOfPoints) when is_list(ListOfPoints)->
    lists:map(fun(Point) -> preparePoint(Point) end, ListOfPoints);
preparePoint({X,Y}) ->
    #{<<"x">> => X, <<"y">> => Y}.

prepareXactors(Xactors) when is_list(Xactors) ->
    lists:map( fun(Xactor) -> prepareXactors(Xactor) end, Xactors );
prepareXactors(Xactor) when is_tuple(Xactor) ->
    #{
        name => element(1, Xactor),
        type => element(2, Xactor),
        %args => element(3, Xactor), %start args, skipped on purpose
        info => element(4, Xactor)
    }.

%%--------------------------------------------------------------------------------------------------
%% converts the template given in a map (with binary keys) into the corresponding record definition
%%--------------------------------------------------------------------------------------------------

%check finished template
checkTemplate(Template) ->
    checkTemplateRecord(Template).
%%    try checkTemplateRecord(Template) of
%%        true -> true
%%    catch
%%        _Exception:Error->
%%            io:format("check error: ~p~n~p~n", [_Exception, Error]),
%%            {error, "Template check failed: " ++ Error}
%%    end.

checkTemplateRecord(R) when is_record(R, ibo_repo_template) ->
    throw_if_false(is_binary(R#ibo_repo_template.name), "template name must be binary"),
    throw_if_false(is_binary(R#ibo_repo_template.description), "template description must be binary"),
    throw_if_false(is_binary(R#ibo_repo_template.startdestination), "template destination must be binary"),
    throw_if_false(is_binary(R#ibo_repo_template.created_by), "template destination must be binary"),
    throw_if_false(is_integer(R#ibo_repo_template.version) andalso 0 <  R#ibo_repo_template.version, "template version must be integer and bigger than 0"),
    throw_if_false(is_integer(R#ibo_repo_template.ttl) andalso 0 <  R#ibo_repo_template.ttl, "template ttl must be integer and bigger than 0"),
    throw_if_false(is_integer(R#ibo_repo_template.startstepnr) andalso 0 <  R#ibo_repo_template.startstepnr, "template startstepnr must be integer and bigger than 0"),

    throw_if_false(is_list(R#ibo_repo_template.groups) andalso length(R#ibo_repo_template.groups) > 0,"template groups must be a list and the size must be at least 1"),
    throw_if_false(lists:all(fun(Group)-> is_binary(Group) end, R#ibo_repo_template.groups), "every group in template groups must be binary"),

    throw_if_false(is_list(R#ibo_repo_template.steps) andalso length(R#ibo_repo_template.steps) > 0,"template steps must be of type list and at least 1"),
    throw_if_false(lists:all(fun(Step)-> checkTemplateRecord(Step) end, R#ibo_repo_template.steps), "all steps in template must be valid templates"),

    throw_if_false(is_record(R#ibo_repo_template.gui, ibo_repo_gui),"template gui must be of record type ibo_repo_gui"),
    throw_if_false(checkTemplateRecord(R#ibo_repo_template.gui), "gui must be checked successfully");

checkTemplateRecord(R) when is_record(R, ibo_xbostep) ->
    throw_if_false(is_binary(R#ibo_xbostep.domain), "ibo_xbostep's domain must be binary"),
    throw_if_false(is_binary(R#ibo_xbostep.local) orelse R#ibo_xbostep.local == undefined,"ibo_xbostep's local must be binary or undefined"),
    throw_if_false(is_binary(R#ibo_xbostep.description),"ibo_xbostep's description must be binary"),
    throw_if_false(is_list(R#ibo_xbostep.commands) andalso length(R#ibo_xbostep.commands) > 0,"ibo_xbostep's commands must be a list with at least one line"),
    throw_if_false(lists:all(fun(Command)-> checkTemplateRecord(Command) end, R#ibo_xbostep.commands), "all step commands must be successfully checked");
checkTemplateRecord(R) when is_record(R, ibo_xboline) ->
    throw_if_false(is_atom(R#ibo_xboline.library), "ibo_xboline's library must be of type atom"),
    throw_if_false(is_atom(R#ibo_xboline.command), "ibo_xboline's command must be of type atom");
checkTemplateRecord(R) when is_record(R, ibo_repo_gui) ->
    throw_if_false(is_list(R#ibo_repo_gui.steps) andalso length(R#ibo_repo_gui.steps) > 0,"ibo_repo_gui's steps must be of type list and have at least one step"),
    throw_if_false(lists:all(fun(Step)-> checkTemplateRecord(Step) end, R#ibo_repo_gui.steps), "ibo_repo_gui's steps must all be checked successfully"),
    throw_if_false(is_tuple(R#ibo_repo_gui.start) andalso tuple_size(R#ibo_repo_gui.start) == 2, "ibo_repo_gui's start must bea tuple with size 2"),
    throw_if_false(checkPoint(element(1, R#ibo_repo_gui.start)) andalso is_list(element(2, R#ibo_repo_gui.start)),"ibo_repo_gui's start first tuple element must be a point, while the second tuple element must be a list"),
    throw_if_false(lists:all(fun(Point)-> checkPoint(Point) end, element(2, R#ibo_repo_gui.start)),"ibo_repo_gui's start second tuple must be a list of points"),
    throw_if_false(checkPoint(R#ibo_repo_gui.endpoint),"ibo_repo_gui's endpoint must be of type point");
checkTemplateRecord(R) when is_record(R, ibo_repo_gui_step) ->
    throw_if_false(checkPoint(R#ibo_repo_gui_step.position), "ibo_repo_gui_step's position must be of type point"),
    throw_if_false(is_list(R#ibo_repo_gui_step.transitions) andalso length(R#ibo_repo_gui_step.transitions) > 0, "ibo_repo_gui_step's transitions must be a list and have at least one transition"),
    throw_if_false(lists:all(fun(Transition)-> checkTemplateRecord(Transition) end, R#ibo_repo_gui_step.transitions), "ibo_repo_gui_step's transitions must be valid transitions");
checkTemplateRecord(R) when is_record(R, ibo_repo_gui_transition) ->
    throw_if_false(R#ibo_repo_gui_transition.destination == finish orelse R#ibo_repo_gui_transition.destination > 0, "ibo_repo_gui_transition's destination must either be finish or an integer > 0 "),
    throw_if_false(is_list(R#ibo_repo_gui_transition.vertices),"ibo_repo_gui_transition's vertices must be of type list"),
    throw_if_false(lists:all(fun(Point)-> checkPoint(Point) end, R#ibo_repo_gui_transition.vertices), "all transition's vertices must be of type point").
checkPoint({X,Y}) when is_integer(X), is_integer(Y) -> true;
checkPoint(_) -> false.


% convert template
prepareTemplate(Map, Creator) when is_map(Map) ->
    %io:format("TemplateMap: ~n~p~n", [Map]),

    RepoTemplate = helper:map_to_record_lenient_atombin(Map, record_info(fields, ibo_repo_template), ibo_repo_template),
    %io:format("TemplateRecord: ~n~p~n", [RepoTemplate]),

    RepoTemplate#ibo_repo_template{
        steps = prepareTemplateSteps(RepoTemplate#ibo_repo_template.steps),
        gui = prepareTemplateGUI(RepoTemplate#ibo_repo_template.gui),
        created_by = Creator
    }.

prepareTemplateSteps(ListOfSteps) ->
    lists:map(fun(Step) ->
        StepRecord = helper:map_to_record_lenient_atombin(Step, record_info(fields, ibo_xbostep), ibo_xbostep),
        StepRecord#ibo_xbostep{commands = prepareTemplateStepCommands(StepRecord#ibo_xbostep.commands)}
              end, ListOfSteps).

%%prepareTemplateSteps(ListOfSteps) ->
%%    prepareTemplateSteps(ListOfSteps, []).
%%prepareTemplateSteps([], Accu) ->
%%    lists:reverse(Accu);
%%prepareTemplateSteps([Step | Steps], Accu) ->
%%    StepRecord = helper:map_to_record_lenient_atombin(Step, record_info(fields, ibo_xbostep), ibo_xbostep),
%%    prepareTemplateSteps(Steps, [StepRecord#ibo_xbostep{commands = prepareTemplateStepCommands(StepRecord#ibo_xbostep.commands)} | Accu]).

prepareTemplateStepCommands(ListOfCommands) ->
    lists:map(fun(Command) ->
        xboline_binary_to_atom(helper:map_to_record_lenient_atombin(Command, record_info(fields, ibo_xboline), ibo_xboline))
              end, ListOfCommands).

%%prepareTemplateStepCommands(ListOfCommands) ->
%%    prepareTemplateStepCommands(ListOfCommands, []).
%%prepareTemplateStepCommands([], Accu) ->
%%    lists:reverse(Accu);
%%prepareTemplateStepCommands([Command | Commands], Accu) ->
%%    CommandRecord = xboline_binary_to_atom(helper:map_to_record_lenient_atombin(Command, record_info(fields, ibo_xboline), ibo_xboline)),
%%    prepareTemplateStepCommands(Commands, [CommandRecord | Accu]).

prepareTemplateGUI(GuiMap) ->
    GuiRecord = helper:map_to_record_lenient_atombin(GuiMap, record_info(fields, ibo_repo_gui), ibo_repo_gui),
    GuiRecord#ibo_repo_gui{
        steps = prepareTemplateGUIsteps(GuiRecord#ibo_repo_gui.steps),
        start = prepareTemplateGUIstart(GuiRecord#ibo_repo_gui.start),
        endpoint = pointmap_to_tuple(GuiRecord#ibo_repo_gui.endpoint)
    }.

prepareTemplateGUIsteps(ListOfGuiSteps) ->
    lists:map(fun(Step) ->
        GuiStepRecord = helper:map_to_record_lenient_atombin(Step, record_info(fields, ibo_repo_gui_step), ibo_repo_gui_step),
        GuiStepRecord#ibo_repo_gui_step{
            transitions = prepareTemplateGuiTransitions(GuiStepRecord#ibo_repo_gui_step.transitions),
            position = pointmap_to_tuple(GuiStepRecord#ibo_repo_gui_step.position)
        }end, ListOfGuiSteps).

%%prepareTemplateGUIsteps(ListOfGuiSteps) ->
%%    prepareTemplateGUIsteps(ListOfGuiSteps, []).
%%prepareTemplateGUIsteps([], Accu) ->
%%    lists:reverse(Accu);
%%prepareTemplateGUIsteps([Step | Steps], Accu) ->
%%    GuiStepRecord = helper:map_to_record_lenient_atombin(Step, record_info(fields, ibo_repo_gui_step), ibo_repo_gui_step),
%%    prepareTemplateGUIsteps(Steps, [GuiStepRecord#ibo_repo_gui_step{
%%        transitions = prepareTemplateGuiTransitions(GuiStepRecord#ibo_repo_gui_step.transitions),
%%        position = pointmap_to_tuple(uiStepRecord#ibo_repo_gui_step.position)
%%    } | Accu]).

prepareTemplateGuiTransitions(ListOfTransitions) ->
    lists:map(fun(Transition) ->
        GT1 = helper:map_to_record_lenient_atombin(Transition, record_info(fields, ibo_repo_gui_transition), ibo_repo_gui_transition),
        GT2 = if
            GT1#ibo_repo_gui_transition.destination == <<"finish">> -> GT1#ibo_repo_gui_transition{destination = finish};
            true -> GT1
        end,
        GT2#ibo_repo_gui_transition{vertices = pointmaplist_to_tupplelist(GT2#ibo_repo_gui_transition.vertices)}
        end, ListOfTransitions).

%%prepareTemplateGuiTransitions(ListOfTransitions) ->
%%    prepareTemplateGuiTransitions(ListOfTransitions, []).
%%prepareTemplateGuiTransitions([], Accu) ->
%%    lists:reverse(Accu);
%%prepareTemplateGuiTransitions([Transition|Transitions], Accu) ->
%%    GT1 = helper:map_to_record_lenient_atombin(Transition, record_info(fields, ibo_repo_gui_transition), ibo_repo_gui_transition),
%%    GT2 = if
%%        GT1#ibo_repo_gui_transition.destination == <<"finish">> -> GT1#ibo_repo_gui_transition{destination = finish};
%%        true -> GT1
%%    end,
%%    GT3 = GT2#ibo_repo_gui_transition{vertices = pointmaplist_to_tupplelist(GT2#ibo_repo_gui_transition.vertices)},
%%    prepareTemplateGuiTransitions(Transitions, [GT3 | Accu]).

prepareTemplateGUIstart(StartMap) when is_map(StartMap)->
    StartPoint = pointmap_to_tuple(maps:get(<<"point">>, StartMap)),
    StartVertices = case maps:is_key(<<"vertices">>, StartMap) of
        true -> lists:map(fun(Point) -> pointmap_to_tuple(Point) end, maps:get(<<"vertices">>, StartMap));
        false -> []
    end,
    {StartPoint, StartVertices}.

pointmaplist_to_tupplelist(PointMapList) when is_list(PointMapList) ->
    lists:map(fun(PointMap) -> pointmap_to_tuple(PointMap) end, PointMapList);
pointmaplist_to_tupplelist(undefined) ->
    [].

pointmap_to_tuple(PointMap) when is_map(PointMap)->
    {maps:get(<<"x">>, PointMap), maps:get(<<"y">>, PointMap)}.

xboline_binary_to_atom(Line) when is_record(Line, ibo_xboline) ->
    Xlibs = xlib:get_libs(),
    AtomLibraries = maps:keys(Xlibs),
    AtomLib = secure_binary_to_atom(Line#ibo_xboline.library, AtomLibraries),
    AtomCommands = maps:keys(maps:get(AtomLib, Xlibs)),
    AtomCommand = secure_binary_to_atom(Line#ibo_xboline.command, AtomCommands),
    Line#ibo_xboline{library = AtomLib, command = AtomCommand}.

secure_binary_to_atom(_BinToConvert, []) ->
    {error, "Given binary could not be converted"};
secure_binary_to_atom(BinToConvert, [Atom | AtomList]) ->
    BinLib = list_to_binary(atom_to_list(Atom)),
    if
        BinToConvert == BinLib -> Atom;
        true -> secure_binary_to_atom(BinToConvert, AtomList)
    end.

%%--------------------------------------------------------------------------------------------------
%% Helper
%%--------------------------------------------------------------------------------------------------
throw_if_false(Expression,ThrowReason) ->
    case Expression of
        true ->
            true;
        _Else ->
            throw(ThrowReason)
    end.
throw_if_true(Expression,ThrowReason)->
    throw_if_false(not Expression,ThrowReason).