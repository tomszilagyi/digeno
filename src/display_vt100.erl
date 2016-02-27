-module(display_vt100).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').

-behaviour(digeno_display).

%% digeno display callback functions
-export([init/1,
         update_workers/1,
         update_converg/2,
         update_status/4]).

%% This is a minimalistic display module for DiGenO, meant to be run
%% in a vt100-compatible terminal (eg. xterm).

init(CbMod) ->
    io:format("\e[2J\e[0;0f"),
    io:format("DiGenO running with callback module: ~p~n", [CbMod]),
    ok.

update_workers(WorkerNodes) ->
    print_worker_nodes(WorkerNodes),
    ok.

update_converg(Reductions, BestFitness) ->
    io:format("\e[11;1f\e[7mConvergence\e[0m~n~11B~n~11.6f\e[K~n", [Reductions, BestFitness]),
    ok.

update_status(Reductions, PopulationSize,
              {WorstInst, WorstResult, WorstFitness},
              {BestInst, BestResult, BestFitness}) ->
    io:format("\e[4;1f" ++
              "Reductions: ~B~nPopulation: ~B~n~n" ++
              "\e[7m           Fitness      Result   Instance\e[0m~n" ++
              "Worst:  ~10s  ~10s   ~s\e[K~n" ++
              " Best:  ~10s  ~10s   ~s\e[K~n",
              [Reductions, PopulationSize,
               utils:format_float(WorstFitness, 10, 5), WorstResult, WorstInst,
               utils:format_float(BestFitness, 10, 5), BestResult, BestInst]),
    ok.

%% private functions

print_worker_nodes(Workers) ->
    SumCores = lists:sum([Cores || {_Node, Cores, _Pids} <- Workers]),
    io:format("\e[15;0f\e[7mWorker nodes (~B)\e[0m\e[K", [SumCores]),
    pwn_1(lists:sort(fun({NA,_,_},{NB,_,_}) -> NA =< NB end, Workers), 16).

pwn_1([], Row) ->
    io:format("\e[~B;0f\e[K", [Row]);
pwn_1([{Node, Cores, _Pids}|RestWorkers], Row) ->
    io:format("\e[~B;0f~s (~B)\e[K", [Row, Node, Cores]),
    pwn_1(RestWorkers, Row+1).
