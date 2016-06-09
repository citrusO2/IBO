%%%-------------------------------------------------------------------
%%% @author Florian
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     General Helper Module
%%% @end
%%% Created : 09. Jun 2016 21:08
%%%-------------------------------------------------------------------
-module(helper).
-author("Florian").

%% API
-export([map_to_record_strict/3, map_to_record_lenient/3, map_to_server_state_strict/2,map_to_server_state_lenient/2, are_lists_disjoint/2, has_group_permission/2, binary_list_to_string/2]).

%% creates the internal server state from a given map
-spec map_to_server_state_strict(Map :: map(), RecordInfo :: tuple() ) -> tuple().  % return type says tuple, but it is of record type '#state{}'
%% typical call: State = helper:map_to_server_state(Args, record_info(fields, state)),
map_to_server_state_strict(Map, RecordInfo) ->
    map_to_record_strict(Map, RecordInfo, state).

%% like strict, but does not crash and writes undefined for the field instead
map_to_server_state_lenient(Map, RecordInfo) ->
    map_to_record_lenient(Map, RecordInfo, state).

map_to_record_strict(Map, RecordInfo, RecordName) ->
    lists:foldl(fun(Elem, AccIn) ->
        case maps:is_key(Elem, Map) of
            true ->
                Value = maps:get(Elem, Map),
                erlang:append_element(AccIn,Value);
            false ->
                throw("Cannot strictly convert map to record " ++ atom_to_list(RecordName) ++ ", map does not contain all necessary elements")
        end
                end,{RecordName},RecordInfo).

map_to_record_lenient(Map, RecordInfo, RecordName) ->
    lists:foldl(fun(Elem, AccIn) ->
        Value = maps:get(Elem, Map, undefined),
        erlang:append_element(AccIn,Value)
                end,{RecordName},RecordInfo).

%% checks if two lists are disjoint (=share no common element)
are_lists_disjoint(List1, List2) ->
    ordsets:is_disjoint(ordsets:from_list(List1), ordsets:from_list(List2)).

%% checks, if no group in GroupsToCheck is a member of AllowedGroups
has_group_permission(GroupsToCheck, AllowedGroups) ->
    not are_lists_disjoint(GroupsToCheck, AllowedGroups).

%% converts a list of binaries to a string with a separator
binary_list_to_string([Binary|BinaryListRest], Separator) ->
    binary_list_to_string(BinaryListRest, Separator, Binary).

binary_list_to_string([], _Separator, Accu)->
    erlang:binary_to_list(Accu);
binary_list_to_string([Bin | Bins], Separator, Accu) ->
    binary_list_to_string(Bins,Separator,erlang:list_to_binary([Accu,Separator,Bin])).
