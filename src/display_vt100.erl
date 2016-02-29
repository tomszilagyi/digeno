-module(display_vt100).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').

-behaviour(digeno_display).

%% digeno display callback functions
-export([init/1,
         update_workers/2,
         update_converg/3,
         update_status/5]).

%% This is a minimalistic display module for DiGenO, meant to be run
%% in a vt100-compatible terminal (eg. xterm).

-record(state, {log_fd, converg_pos}).

init(CbMod) ->
    io:format("\e[2J\e[0;0f"),
    io:format("DiGenO running with callback module: ~p~n", [CbMod]),
    io:format("\e[12;1f\e[7mConvergence\e[0m"),
    {ok, FD} = file:open("converg.log", [write]),
    {ok, #state{log_fd=FD, converg_pos={0, 0}}}.

update_workers(WorkerNodes, State) ->
    print_worker_nodes(WorkerNodes),
    State.

update_converg(Reductions, BestFitness, #state{log_fd=FD, converg_pos=Pos0}=State) ->
    Pos = print_converg(Reductions, BestFitness, Pos0),
    io:format(FD, "~B, ~g~n", [Reductions, BestFitness]),
    State#state{converg_pos=Pos}.

update_status(Reductions, PopulationSize,
              {WorstInst, WorstResult, WorstFitness},
              {BestInst, BestResult, BestFitness},
              State) ->
    io:format("\e[4;1f" ++
              "Reductions: ~B~nPopulation: ~B~n~n" ++
              "\e[7m           Fitness      Result   Instance\e[0m~n" ++
              "Worst:  ~10s  ~10s   ~s\e[K~n" ++
              " Best:  ~10s  ~10s   ~s\e[K~n",
              [Reductions, PopulationSize,
               utils:format_float(WorstFitness, 10, 5), WorstResult, WorstInst,
               utils:format_float(BestFitness, 10, 5), BestResult, BestInst]),
    State.

%% private functions

print_converg(Reductions, BestFitness, {VirtRow, VirtCol}) ->
    {ScrRow, ScrCol} = screen_pos({VirtRow, VirtCol}),
    io:format("\e[~B;~Bf~9B~10.6f", [ScrRow, ScrCol, Reductions, BestFitness]),
    NextRow = (VirtRow + 1) rem 10,
    NextCol = if NextRow == 0 -> (VirtCol + 1) rem 4;
                 true -> VirtCol
              end,
    {NextScrRow, NextScrCol} = screen_pos({NextRow, NextCol}),
    io:format("\e[~B;~Bf                    ", [NextScrRow, NextScrCol]),
    {NextRow, NextCol}.

screen_pos({Row, Col}) -> {Row + 13, 20 * Col}.

print_worker_nodes(Workers) ->
    SumCores = lists:sum([Cores || {_Node, Cores, _Pids} <- Workers]),
    io:format("\e[25;0f\e[7mWorker nodes (~B)\e[0m\e[K", [SumCores]),
    pwn_1(lists:sort(fun({NA,_,_},{NB,_,_}) -> NA =< NB end, Workers), 26).

pwn_1([], Row) ->
    io:format("\e[~B;0f\e[K", [Row]);
pwn_1([{Node, Cores, _Pids}|RestWorkers], Row) ->
    io:format("\e[~B;0f~s (~B)\e[K", [Row, Node, Cores]),
    pwn_1(RestWorkers, Row+1).
