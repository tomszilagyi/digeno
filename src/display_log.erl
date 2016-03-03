-module(display_log).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').

-behaviour(digeno_display).

%% digeno display callback functions
-export([init/1,
         update_workers/2,
         update_converg/3,
         update_status/5]).

%% This is a minimal display module for DiGenO, meant to provide an
%% example (and aid with debugging).

-record(state, {log_fd}).

init(CbMod) ->
    io:format("DiGenO running with callback module: ~p~n", [CbMod]),
    {ok, FD} = file:open("converg.log", [write]),
    {ok, #state{log_fd=FD}}.

update_workers(Workers, State) ->
    SumCores = lists:sum([Cores || {_Node, Cores, _Pids} <- Workers]),
    io:format("Worker nodes (total cores: ~B): ", [SumCores]),
    SL = [io_lib:format("~p (~B)", [Node, Cores])  || {Node, Cores, _Pids} <- Workers],
    io:format("~s~n", [string:join(SL, ", ")]),
    State.

update_converg(Reductions, BestFitness, #state{log_fd=FD}=State) ->
    io:format(FD, "~B, ~.8g~n", [Reductions, BestFitness]),
    State.

update_status(Reductions, PopulationSize,
              {_WorstInst, _WorstResult, _WorstFitness},
              {BestInst, BestResult, BestFitness},
              State) ->
    io:format("~B reds; pop: ~B; best fit: ~f result: ~s instance: ~s~n",
              [Reductions, PopulationSize, BestFitness, BestResult, BestInst]),
    State.
