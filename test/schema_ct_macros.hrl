%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Dez 2015 21:33
%%%-------------------------------------------------------------------
-author("Florian").

-define(TESTSCHEMA1,
    #{
        <<"title">> => <<"Marketing Budget - Decision">>,
        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"reason">>, <<"yesno">>]
    }).

-define(TESTSCHEMA2, % wrong description
    #{
        <<"title">> => <<"Marketing Budget - Decision">>,
        <<"description">> => 2312,
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"reason">>, <<"yesno">>]
    }).

-define(TESTSCHEMA3, % wrong field type
    #{
        <<"title">> => <<"Marketing Budget - Decision">>,
        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
        <<"type">> => <<"miau">>,
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"reason">>, <<"yesno">>]
    }).

-define(TESTSCHEMA4, % wrong title type
    #{
        <<"title">> => true,
        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"reason">>, <<"yesno">>]
    }).

-define(TESTSCHEMA5, % wrong enum field type
    #{
        <<"title">> => <<"Marketing Budget - Decision">>,
        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [ 3, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"reason">>, <<"yesno">>]
    }).

-define(TESTSCHEMA6, % wrong required element
    #{
        <<"title">> => <<"Marketing Budget - Decision">>,
        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"wuhaha">>, <<"yesno">>]
    }).

-define(TESTSCHEMA7,    % if type object it has to have the field properties
    #{
        <<"title">> => <<"Marketing Budget - Decision">>,
        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
        <<"type">> => <<"object">>,
        <<"items">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"reason">>, <<"yesno">>]
    }).

-define(TESTSCHEMA8,    % if type array it has to have the field items
    #{
        <<"title">> => <<"Product set">>,
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"title">> => <<"Marketing Budget - Decision">>,
            <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"reason">> => #{
                    <<"title">> => <<"Reason">>,
                    <<"description">> => <<"The reason for your decision">>,
                    <<"type">> => <<"string">>
                },
                <<"yesno">> => #{
                    <<"title">> => <<"Decide">>,
                    <<"description">> => <<"tick your decision">>,
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
                }
            },
            <<"required">> => [<<"reason">>, <<"yesno">>]
        }
    }
).

-define(TESTSCHEMA9,    % if type array it must not have field properties
    #{
        <<"title">> => <<"Product set">>,
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"title">> => <<"Marketing Budget - Decision">>,
            <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"reason">> => #{
                    <<"title">> => <<"Reason">>,
                    <<"description">> => <<"The reason for your decision">>,
                    <<"type">> => <<"string">>
                },
                <<"yesno">> => #{
                    <<"title">> => <<"Decide">>,
                    <<"description">> => <<"tick your decision">>,
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
                }
            },
            <<"required">> => [<<"reason">>, <<"yesno">>]
        },
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        }
    }
).

-define(TESTSCHEMA10,    %  type should be array
    #{
        <<"title">> => <<"Product set">>,
        <<"type">> => <<"object">>,
        <<"items">> => #{
            <<"title">> => <<"Marketing Budget - Decision">>,
            <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"reason">> => #{
                    <<"title">> => <<"Reason">>,
                    <<"description">> => <<"The reason for your decision">>,
                    <<"type">> => <<"string">>
                },
                <<"yesno">> => #{
                    <<"title">> => <<"Decide">>,
                    <<"description">> => <<"tick your decision">>,
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
                }
            },
            <<"required">> => [<<"reason">>, <<"yesno">>]
        },
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        }
    }
).

-define(TESTSCHEMA11,    %  type should be array
    #{
        <<"title">> => <<"Product set">>,
        <<"type">> => <<"string">>,
        <<"items">> => #{
            <<"title">> => <<"Marketing Budget - Decision">>,
            <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"reason">> => #{
                    <<"title">> => <<"Reason">>,
                    <<"description">> => <<"The reason for your decision">>,
                    <<"type">> => <<"string">>
                },
                <<"yesno">> => #{
                    <<"title">> => <<"Decide">>,
                    <<"description">> => <<"tick your decision">>,
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
                }
            },
            <<"required">> => [<<"reason">>, <<"yesno">>]
        },
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        }
    }
).
-define(TESTSCHEMA12,
    #{
        <<"title">> => <<"Marketing Budget - Decision">>,
        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>,
                <<"minLength">> => 20
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"reason">>, <<"yesno">>]
    }).

-define(TESTSCHEMA13, % only one required element
    #{
        <<"title">> => <<"Marketing Budget - Decision">>,
        <<"description">> => <<"Approve the current marketing budget of 250.000 EUR">>,
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"reason">> => #{
                <<"title">> => <<"Reason">>,
                <<"description">> => <<"The reason for your decision">>,
                <<"type">> => <<"string">>
            },
            <<"yesno">> => #{
                <<"title">> => <<"Decide">>,
                <<"description">> => <<"tick your decision">>,
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"no">>, <<"yes">>, <<"maybe">>]
            }
        },
        <<"required">> => [<<"yesno">>]
    }).