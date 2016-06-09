%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Mär 2016 20:45
%%%-------------------------------------------------------------------
-module(repo_exampletemplates).
-author("Florian").

-include("repo_records.hrl").

%% API
-export([install/1]).

marketingbudget_template() ->
    #ibo_repo_template{
        name = <<"Accept or deny the marketing budget">>,
        version = 1,
        ttl = 60*60*24*14,
        steps = [
            #ibo_xbostep{
                domain = <<"box_server">>,
                local = <<"marketing">>,
                description = <<"Set a marketing budget">>,
                commands = [
                    #ibo_xboline{
                        library = xlib_box,
                        command = init,
                        args = [
                            #{
                                <<"title">> => <<"Marketing Budget - Amount">>,
                                <<"description">> => <<"Set the needed marketing budget">>,
                                <<"type">> => <<"object">>,
                                <<"properties">> => #{
                                    <<"budget">> => #{
                                        <<"title">> => <<"Budget">>,
                                        <<"description">> => <<"Set the budget">>,
                                        <<"type">> => <<"number">>
                                    },
                                    <<"reason">> => #{
                                        <<"title">> => <<"Reason">>,
                                        <<"description">> => <<"Additional information why the budget should be approved">>,
                                        <<"type">> => <<"string">>
                                    }
                                },
                                <<"required">> => [<<"budget">>, <<"reason">>]
                            }
                        ]
                    },
                    #ibo_xboline{library = xlib_basic, command = send, args = [2, "box_server"]}
                ]},
            #ibo_xbostep{
                domain = <<"box_server">>,
                local = <<"marketing_head">>,
                description = <<"Approval of the marketing budget">>,
                commands = [
                    #ibo_xboline{
                        library = xlib_box,
                        command = init,
                        args = [
                            #{
                                <<"title">> => <<"Marketing Budget - Decision">>,
                                <<"description">> => <<"Approve the current marketing budget of ">>,
                                <<"type">> => <<"object">>,
                                <<"properties">> => #{
                                    <<"yesno">> => #{
                                        <<"title">> => <<"Decide">>,
                                        <<"description">> => <<"tick your decision">>,
                                        <<"type">> => <<"string">>,
                                        <<"enum">> => [<<"no">>, <<"yes">>, <<"rework">>]
                                    },
                                    <<"reason">> => #{
                                        <<"title">> => <<"Reason">>,
                                        <<"description">> => <<"The reason for your decision">>,
                                        <<"type">> => <<"string">>
                                    }
                                },
                                <<"required">> => [<<"reason">>, <<"yesno">>]
                            }, fun(Taskdetailstemplate, OtherStepdata) ->
                                [DataStep16|_] = lists:filter( fun(Element) -> Element#ibo_xbostepdata.stepnr =:= 1 orelse Element#ibo_xbostepdata.stepnr =:= 6 end , OtherStepdata),
                                Budget = maps:get(<<"budget">>,DataStep16#ibo_xbostepdata.vars),
                                Reason =  maps:get(<<"reason">>,DataStep16#ibo_xbostepdata.vars),
                                NewDescription = binary:list_to_bin([maps:get(<<"description">>, Taskdetailstemplate) , integer_to_binary(round(Budget)) , <<", because: ">> , Reason]),
                                Taskdetailstemplate#{<<"description">> := NewDescription} end
                        ]
                    }, #ibo_xboline{library = xlib_basic, command = cjump, args = [5, fun(StepData, _OtherStepData) ->
                        case maps:find(<<"yesno">>, StepData#ibo_xbostepdata.vars) of {ok, <<"yes">>} -> true; _Else ->
                            false end end]}
                    , #ibo_xboline{library = xlib_basic, command = cjump, args = [6, fun(StepData, _OtherStepData) ->
                        case maps:find(<<"yesno">>, StepData#ibo_xbostepdata.vars) of {ok, <<"rework">>} -> true; _Else ->
                            false end end]},
                    #ibo_xboline{library = xlib_basic, command = send, args = [3, "box_server"]},
                    #ibo_xboline{library = xlib_basic, command = send, args = [4, "box_server"]},
                    #ibo_xboline{library = xlib_basic, command = send, args = [6, "box_server"]}
                ]},
            #ibo_xbostep{
                domain = <<"box_server">>,
                local = <<"marketing">>,
                description = <<"Marketing Budget Decision">>,
                commands = [#ibo_xboline{
                    library = xlib_box,
                    command = init,
                    args = [
                        #{
                            <<"title">> => <<"Marketing Budget - Denied">>,
                            <<"description">> => <<"Reason why Budget was Denied: ">>,
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"ok">> => #{
                                    <<"title">> => <<"ok">>,
                                    <<"description">> => <<"confirm receive of message">>,
                                    <<"type">> => <<"string">>,
                                    <<"enum">> => [<<"ok">>]
                                }
                            },
                            <<"required">> => [<<"ok">>]
                        },fun(Taskdetailstemplate, OtherStepdata) ->
                            [DataStep2|_] = lists:filter( fun(Element) -> Element#ibo_xbostepdata.stepnr =:= 2 end , OtherStepdata),
                            Reason = maps:get(<<"reason">>,DataStep2#ibo_xbostepdata.vars),
                            NewDescription = binary:list_to_bin([ maps:get(<<"description">>, Taskdetailstemplate) , Reason]),
                            Taskdetailstemplate#{<<"description">> := NewDescription} end
                    ]
                }, #ibo_xboline{library = xlib_basic, command = finish}
                ]
            },
            #ibo_xbostep{
                domain = <<"box_server">>,
                local = <<"marketing">>,
                description = <<"Marketing Budget Decision">>,
                commands = [#ibo_xboline{
                    library = xlib_box,
                    command = init,
                    args = [
                        #{
                            <<"title">> => <<"Marketing Budget - Approved">>,
                            <<"description">> => <<"Reason why Budget was Approved: ">>,
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"ok">> => #{
                                    <<"title">> => <<"ok">>,
                                    <<"description">> => <<"confirm receive of message">>,
                                    <<"type">> => <<"string">>,
                                    <<"enum">> => [<<"ok">>]
                                }
                            },
                            <<"required">> => [<<"ok">>]
                        },fun(Taskdetailstemplate, OtherStepdata) ->
                            [DataStep2|_] = lists:filter( fun(Element) -> Element#ibo_xbostepdata.stepnr =:= 2 end , OtherStepdata),
                            Reason = maps:get(<<"reason">>,DataStep2#ibo_xbostepdata.vars),
                            NewDescription = binary:list_to_bin([maps:get(<<"description">>, Taskdetailstemplate), Reason]),
                            Taskdetailstemplate#{<<"description">> := NewDescription} end
                    ]
                }, #ibo_xboline{library = xlib_basic, command = send, args = [5, "box_server"]}
                ]
            },
            #ibo_xbostep{
                domain = <<"box_server">>,
                local = <<"accounting">>,
                description = <<"Marketing Budget Decision">>,
                commands = [#ibo_xboline{
                    library = xlib_box,
                    command = init,
                    args = [
                        #{
                            <<"title">> => <<"Marketing Budget - Approved">>,
                            <<"description">> => <<"The following marketing budget has been approved: ">>,
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"ok">> => #{
                                    <<"title">> => <<"ok">>,
                                    <<"description">> => <<"confirm receive of message">>,
                                    <<"type">> => <<"string">>,
                                    <<"enum">> => [<<"ok">>]
                                }
                            },
                            <<"required">> => [<<"ok">>]
                        },fun(Taskdetailstemplate, OtherStepdata) ->
                            [DataStep2|_] = lists:filter( fun(Element) -> Element#ibo_xbostepdata.stepnr =:= 2 end , OtherStepdata),
                            Reason = maps:get(<<"reason">>,DataStep2#ibo_xbostepdata.vars),
                            [DataStep16|_] = lists:filter( fun(Element) -> Element#ibo_xbostepdata.stepnr =:= 1 orelse Element#ibo_xbostepdata.stepnr =:= 6 end , OtherStepdata),
                            Budget = maps:get(<<"budget">>,DataStep16#ibo_xbostepdata.vars),
                            NewDescription = binary:list_to_bin([maps:get(<<"description">>, Taskdetailstemplate), integer_to_binary(round(Budget)), <<", because: ">>, Reason]),
                            Taskdetailstemplate#{<<"description">> := NewDescription} end
                    ]
                }, #ibo_xboline{library = xlib_basic, command = finish}
                ]
            },
            #ibo_xbostep{
                domain = <<"box_server">>,
                local = <<"marketing">>,
                description = <<"Rework Marketing Budget">>,
                commands = [
                    #ibo_xboline{
                        library = xlib_box,
                        command = init,
                        args = [
                            #{
                                <<"title">> => <<"Marketing Budget - Rework">>,
                                <<"description">> => <<"Rework the Budget: ">>,
                                <<"type">> => <<"object">>,
                                <<"properties">> => #{
                                    <<"budget">> => #{
                                        <<"title">> => <<"Budget">>,
                                        <<"description">> => <<"Set the budget">>,
                                        <<"type">> => <<"number">>
                                    },
                                    <<"reason">> => #{
                                        <<"title">> => <<"Reason">>,
                                        <<"description">> => <<"Additional information why the budget should be approved">>,
                                        <<"type">> => <<"string">>
                                    }
                                },
                                <<"required">> => [<<"budget">>, <<"reason">>]
                            },fun(Taskdetailstemplate, OtherStepdata) ->
                                [DataStep2|_] = lists:filter( fun(Element) -> Element#ibo_xbostepdata.stepnr =:= 2 end , OtherStepdata),
                                Reason = maps:get(<<"reason">>,DataStep2#ibo_xbostepdata.vars),
                                NewDescription = binary:list_to_bin([maps:get(<<"description">>, Taskdetailstemplate), Reason]),
                                Taskdetailstemplate#{<<"description">> := NewDescription} end
                        ]
                    },
                    #ibo_xboline{library = xlib_basic, command = send, args = [2, "box_server"]}
                ]}
        ],
        groups = [<<"marketing">>],
        startstepnr = 1,
        startdestination = "box_server",
        transform = fun(IBO,_Args) -> IBO end
    }.

install(RepoServer) ->
    Templates = [marketingbudget_template()],
    lists:foreach(fun(Template) -> repo_server:store_template(RepoServer, Template) end, Templates),
    ok.

