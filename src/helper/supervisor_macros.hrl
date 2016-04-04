%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2016 20:52
%%%-------------------------------------------------------------------
-author("Florian").

% old way of defining child_spec()
%%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
%%-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

% new prefered versions with maps
-define(CHILD(Id, Type), #{
    id => Id,                       % mandatory
    start => {Id, start_link, []},  % mandatory
    restart => permanent,           % optional
    shutdown => 5000,               % optional
    type => Type,                   % optional
    modules => [Id]}                % optional
).

-define(CHILD(Id, Type, Args), #{
    id => Id,                       % mandatory
    start => {Id, start_link, Args},  % mandatory
    restart => permanent,           % optional
    shutdown => 5000,               % optional
    type => Type,                   % optional
    modules => [Id]}                % optional
).

-define(TRANSCHILD(Id, Type), #{
    id => Id,                       % mandatory
    start => {Id, start_link, []},  % mandatory
    restart => transient,           % optional
    shutdown => 5000,               % optional
    type => Type,                   % optional
    modules => [Id]}                % optional
).

-define(TRANSCHILD(Id, Type, Args), #{
    id => Id,                       % mandatory
    start => {Id, start_link, Args},  % mandatory
    restart => transient,           % optional
    shutdown => 5000,               % optional
    type => Type,                   % optional
    modules => [Id]}                % optional
).

-define(FLAGS(Strategy,Intensity,Period),#{
    strategy => Strategy,
    intensity => Intensity,
    period => Period
}).