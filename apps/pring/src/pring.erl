-module(pring).

-export([ping/1, all_members/0, print_ring/0, broadcast_ring/0, transfer_node/2]).

ping(NodeName) ->
    partisan_peer_service:forward_message(NodeName, pring_handler, ping).

all_members() ->
    {ok, Ring} = pring_broadcast_handler:get_ring(),
    riak_core_ring:all_members(Ring).

print_ring() ->
    {ok, Ring} = pring_broadcast_handler:get_ring(),
    riak_core_ring:pretty_print(Ring, [legend]).

broadcast_ring() ->
    pring_broadcast_handler:broadcast_ring().

transfer_node(Idx, ToNode) ->
    pring_broadcast_handler:ring_apply(fun (Ring0) ->
                                               Ring1 = riak_core_ring:transfer_node(Idx, ToNode, Ring0),
                                               {ok, Ring1, ok}
                                       end).
