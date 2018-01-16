%%%-------------------------------------------------------------------
%% @doc pring top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pring_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Handler = { pring_handler,
                   { pring_handler, start_link, []},
                   permanent, 5000, worker, [pring_handler]},
    {ok, { {one_for_all, 0, 1}, [Handler]} }.

%%====================================================================
%% Internal functions
%%====================================================================
