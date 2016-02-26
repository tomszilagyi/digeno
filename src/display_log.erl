-module(display_log).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').

-behaviour(digeno_display).

%% digeno display callback functions
-export([init/1,
         update_workers/1,
         update_converg/2,
         update_status/4]).

%% This is a minimal display module for DiGenO, meant to provide an
%% example (and aid with debugging).

init(CbMod) ->
    io:format("DiGenO running with callback module: ~p~n", [CbMod]),
    ok.

update_workers(Workers) ->
    SumCores = lists:sum([Cores || {_Node, Cores, _Pids} <- Workers]),
    io:format("Worker nodes (total cores: ~B): ", [SumCores]),
    SL = [io_lib:format("~p (~B)", [Node, Cores])  || {Node, Cores, _Pids} <- Workers],
    io:format("~s~n", [string:join(SL, ", ")]),
    ok.

update_converg(Reductions, BestFitness) ->
    io:format("Converg: ~B -> ~f~n", [Reductions, BestFitness]),
    ok.

update_status(Reductions, PopulationSize,
              {_WorstInst, _WorstResult, _WorstFitness},
              {BestInst, BestResult, BestFitness}) ->
    io:format("~B reds; pop: ~B; best fit: ~f result: ~s instance: ~s~n",
              [Reductions, PopulationSize, BestFitness, BestResult, BestInst]),
    ok.
