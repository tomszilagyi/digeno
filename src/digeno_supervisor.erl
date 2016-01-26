-module(digeno_supervisor).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(supervisor).
-export([start/2, start_link/2, init/1]).

% start by supervisor
start(_StartType, StartArgs) ->
    supervisor:start_link({local,digeno_supervisor}, ?MODULE, StartArgs).

% start by supervisor
start_link(_StartType, StartArgs) ->
    supervisor:start_link({local,digeno_supervisor}, ?MODULE, StartArgs).

% supervisor callback functions
init(_StartArgs) ->
    {ok, CallbackMod} = application:get_env(digeno, callback_module),
    {ok, DisplayMod} = application:get_env(digeno, display_module),
    DiGenO = {digeno_master,
              {digeno_master, start_link, [CallbackMod, DisplayMod]},
              permanent, 2000, worker, [digeno_master]},
    {ok, {{one_for_one, 100, 10}, [DiGenO]}}.
