-module(pring_peer_service).

-export([join/1,
         leave/1,
         on_down/2,
         members/0,
         manager/0,
         stop/0,
         stop/1]).

-export([join_p/1, leave_p/1, members_p/1, connections_p/1]).

join(Node) ->
    partisan_peer_service:join(Node).

leave(Node) ->
    partisan_peer_service:leave(Node).

on_down(Name, Fun) ->
    partisan_default_peer_service_manager:on_down(Name, Fun).

members() ->
    partisan_peer_service:members().

manager() ->
    partisan_peer_service:manager().

stop() ->
    partisan_peer_service:stop("received stop request").

stop(Reason) ->
    partisan_peer_service:stop(Reason).

join_p(Node) -> io:format("~p~n", [join(Node)]).
leave_p(Node) -> io:format("~p~n", [leave(Node)]).
members_p([]) ->
    partisan_peer_service_console:members([]).
connections_p([]) ->
    io:format("~p~n", [partisan_peer_service:connections()]).

