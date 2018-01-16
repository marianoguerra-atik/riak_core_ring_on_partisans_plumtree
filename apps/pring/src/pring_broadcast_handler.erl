-module(pring_broadcast_handler).

-behaviour(partisan_plumtree_broadcast_handler).

%% API
-export([start_link/0,
         start_link/1]).

%% partisan_plumtree_broadcast_handler callbacks
-export([broadcast_data/1,
         merge/2,
         is_stale/1,
         graft/1,
         exchange/1]).

-export([ring_apply/1, get_ring/0, broadcast_ring/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% State record.
-record(state, {ring=nil, node=nil}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Same as start_link([]).
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start and link to calling process.
-spec start_link(map())-> {ok, pid()} | ignore | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Returns from the broadcast message the identifier and the payload.
broadcast_data({Node, Ring}) ->
    {{Node, Ring}, Ring}.

merge({Node, _NodeRing}, Ring) ->
    case is_stale({Node, Ring}) of
        true ->
            false;
        false ->
            gen_server:call(?MODULE, {merge, Node, Ring}, infinity),
            true
    end.

is_stale(NodeAndRing) ->
    gen_server:call(?MODULE, {is_stale, NodeAndRing}, infinity).

graft(NodeAndRing) ->
    gen_server:call(?MODULE, {graft, NodeAndRing}, infinity).

ring_apply(Fn) ->
    gen_server:call(?MODULE, {ring_apply, Fn}, infinity).

get_ring() ->
    gen_server:call(?MODULE, {get_ring}, infinity).

broadcast_ring() ->
    gen_server:call(?MODULE, {broadcast_ring}, infinity).

%% @doc Anti-entropy mechanism.
-spec exchange(node()) -> {ok, pid()}.
exchange(_Peer) ->
    %% Ignore the standard anti-entropy mechanism from plumtree.
    %%
    %% Spawn a process that terminates immediately, because the
    %% broadcast exchange timer tracks the number of in progress
    %% exchanges and bounds it by that limit.
    %%
    Pid = spawn_link(fun() -> ok end),
    {ok, Pid}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(map()) -> {ok, #state{}}.
init(Opts) ->
    %% Seed the process at initialization.
    rand_compat:seed(erlang:phash2([node()]),
                     erlang:monotonic_time(),
                     erlang:unique_integer()),

    RingSize = maps:get(ring_size, Opts, 64),
    GossipVsn = maps:get(gossip_vsn, Opts, 2),
    Node = maps:get(node, Opts, node()),
    Ring = riak_core_ring:fresh(RingSize, Node, GossipVsn),

    {ok, #state{ring=Ring, node=Node}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

%% @private
%% second item of is_stale tuple is the message identifier returned by
%% broadcast_data
handle_call({is_stale, {Node, NodeRing}}, _From, State=#state{ring=Ring}) ->
    Result = riak_core_ring:descends(NodeRing, Ring),
    lager:info("is_stale: ~p -> ~p", [Node, Result]),
    {reply, Result, State};

% graft receives msg identifier and must return message payload
handle_call({graft, {Node, NodeRing}}, _From, State) ->
    lager:info("graft ~p", [Node]),
    Result = NodeRing,
    {reply, {ok, Result}, State};

handle_call({merge, Node, NodeRing}, _From, State=#state{ring=Ring}) ->
    lager:info("merging ring from ~p", [Node]),
    NewRing = case riak_core_ring:reconcile(Ring, NodeRing) of
                  no_change -> Ring;
                  {new_ring, NRing} -> NRing
              end,

    {reply, ok, State#state{ring=NewRing}};

handle_call({broadcast_ring}, _From, State=#state{ring=Ring, node=Node}) ->
    R = partisan_plumtree_broadcast:broadcast({Node, Ring}, pring_broadcast_handler),
    {reply, R, State};

handle_call({get_ring}, _From, State=#state{ring=Ring}) ->
    {reply, {ok, Ring}, State};

handle_call({ring_apply, Fn}, _From, State=#state{ring=Ring}) ->
    {ok, NewRing, Result} = Fn(Ring),
    {reply, {ok, Result}, State#state{ring=NewRing}};

handle_call(Msg, _From, State) ->
    _ = lager:warning("Unhandled messages: ~p", [Msg]),
    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
%% @private
handle_cast(Msg, State) ->
    _ = lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info(Msg, State) ->
    _ = lager:warning("Unhandled messages: ~p", [Msg]),
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> term().
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term() | {down, term()}, #state{}, term()) ->
    {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

