%%%-------------------------------------------------------------------
%% @doc test_lists top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(test_lists_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(MILLISECONDS_IN_SECOND, 1000).
-define(WORKER(I), {I, {I, 'start_link', []}, 'permanent', 5 * ?MILLISECONDS_IN_SECOND, 'worker', [I]}).

-define(CHILDREN, [
    ?WORKER('test_lists')
]).


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
    {ok, { {one_for_all, 0, 1}, ?CHILDREN} }.

%%====================================================================
%% Internal functions
%%====================================================================
