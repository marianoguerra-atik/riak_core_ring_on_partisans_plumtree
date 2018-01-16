-module(pring).

-export([ping/1]).

ping(NodeName) ->
    partisan_peer_service:forward_message(NodeName, pring_handler, ping).
