%%-*- mode: erlang -*-
%% ex: ft=erlang ts=4 sw=4 et

%% @doc ip to listen to for partisan peer service
{mapping, "partisan.peer.ip", "partisan.peer_ip", [
  {datatype, string},
  {default, "{{partisan_peer_ip}}" }
]}.

%% @doc port to listen to for partisan peer service
{mapping, "partisan.peer.port", "partisan.peer_port", [
  {datatype, integer},
  {default, {{partisan_peer_port}} }
]}.

%% @doc backend for peer service operations
{mapping, "partisan.peer.service.manager", "partisan.partisan_peer_service_manager", [
  {datatype, {enum, [static, hyparview, default, clientserver]}},
  {default, hyparview}
]}.


{translation, "partisan.peer_ip",
 fun(Conf) ->
         IPStr = cuttlefish:conf_get("partisan.peer.ip", Conf),
         case inet:parse_address(IPStr) of
            {ok, IP} -> IP;
            {error, Reason} ->
                cuttlefish:invalid("should be an IP address")
         end
 end}.

{translation, "partisan.partisan_peer_service_manager",
 fun(Conf) ->
         case cuttlefish:conf_get("partisan.peer.service.manager", Conf) of
            static -> partisan_static_peer_service_manager;
            hyparview -> partisan_hyparview_peer_service_manager;
            default -> partisan_default_peer_service_manager;
            clientserver -> partisan_client_server_peer_service_manager
         end
 end}.
