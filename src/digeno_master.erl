-module(digeno_master).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(gen_server).
-export([start/2, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% hardcoded default if missing from callback's get_config()
-define(DISPLAY_DECIMATOR, 100).
-define(POPULATION_SIZE, 100).

%% TODO allow multiple instances with the same fitness in the pool

-record(state, {callback_module, display_module, config, rr_pool, n_reds,
                population, workers, terminate}).
-record(work_result, {pid, instance, eval_result}).

start(CbMod, DispMod) ->
    start_link(CbMod, DispMod).

start_link(CbMod, DispMod) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [CbMod, DispMod], []).

init([CbMod, DispMod]) ->
    {Cores, _MaxCores, _LogicalProcs} = utils:count_cores(),
    ok = DispMod:init(CbMod, Cores),
    random:seed(now()),

    %% %% read back last saved state
    %% Filename = "output/tree.bin",
    %% {ok, TreeBin} = file:read_file(Filename),
    %% Tree = binary_to_term(TreeBin),
    Tree = ets:new(digeno_population, [ordered_set, public]),

    %% make sure all modules are loaded (so we can load them onto worker nodes)
    ModFilenames = filelib:wildcard("*.beam", filename:join(filename:dirname(code:priv_dir(digeno)), "ebin")),
    lists:foreach(fun(Fn) -> code:ensure_loaded(list_to_atom(filename:basename(Fn, ".beam"))) end, ModFilenames),

    %% read list of last seen workers, try to connect them & start worker gen_server on them
    case file:consult("digeno_workers.txt") of
	{ok, [WorkerNodes]} -> lists:foreach(fun connect_worker/1, WorkerNodes);
	_ -> ok
    end,
    save_worker_nodes([]),
    DispMod:update_workers([]),

    Pool = lists:map(fun(_) -> spawn_link(fun master/0) end, lists:seq(1, Cores)),
    RRPool = utils:round_robin(Pool),

    Config = CbMod:get_config(),
    {ok, #state{callback_module=CbMod, display_module=DispMod, rr_pool=RRPool,
                n_reds=0, population=Tree, config=Config, workers=[],
                terminate=false}}.

handle_call(Request, From, State) ->
    io:format("~p: handle_call: req = ~p  from = ~p~n", [?MODULE, Request, From]),
    {reply, the_reply, State}.

handle_cast(Msg, State) ->
    io:format("~p: handle_cast: msg = ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(#work_result{}=WorkResult, #state{rr_pool=RRPool0, n_reds=Reds}=State) -> %% process work result
    {PoolPid, RRPool} = utils:round_robin(RRPool0),
    PoolPid ! {process_result, self(), WorkResult, State},
    {noreply, State#state{rr_pool=RRPool, n_reds=Reds+1}};
handle_info({node, Node, Cores, Pids},
            #state{callback_module=CbMod, display_module=DispMod, workers=Workers}=State) -> %% new worker node
    NewWorkers = [{Node, Cores, Pids} | Workers],
    erlang:monitor_node(Node, true),
    %% load our modules on the remote node
    LocalMods = code:all_loaded(),
    RemoteMods = rpc:call(Node, code, all_loaded, []),
    ModsToLoad = LocalMods -- RemoteMods,
    remote_load_modules(Node, ModsToLoad),
    %% start new workload in each of the worker pids (one per core)
    PPid = self(),
    lists:foreach(fun(P) -> spawn_generate(P, PPid, CbMod) end, Pids),
    save_worker_nodes(NewWorkers),
    DispMod:update_workers(NewWorkers),
    {noreply, State#state{workers=NewWorkers}};
handle_info({nodedown, Node}, #state{display_module=DispMod, workers=Workers}=State) -> %% worker node disconnected
    NewWorkers = lists:keydelete(Node, 1, Workers),
    save_worker_nodes(NewWorkers),
    DispMod:update_workers(NewWorkers),
    {noreply, State#state{workers=NewWorkers}};
handle_info(Info, State) ->
    io:format("~p: handle_info: info = ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p: terminate with Reason=~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("~p: code_change~n", [?MODULE]),
    {ok, State}.


connect_worker(Node) ->
    case net_kernel:connect_node(Node) of
        true -> ok = global:sync(),
                rpc:call(Node, digeno_worker, start, []);
        false -> ok
    end.

remote_load_modules(_Node, []) -> ok;
remote_load_modules(Node, [{Mod,_Path} | Rest]) ->
    case code:get_object_code(Mod) of
        {Mod, Bin, _Filename} ->
            %% The fake Filename "DIGENO-REMOVE-ME" ensures that digeno_worker removes
            %% this module when connection is lost with us -- so when we reconnect
            %% we can re-load a different version of it.
            rpc:call(Node, code, load_binary, [Mod, "DIGENO-REMOVE-ME", Bin]);
        _ -> ok
    end,
    remote_load_modules(Node, Rest).

process_result(PPid,
               #work_result{pid=Pid, instance=Instance, eval_result=EvalResult},
               #state{callback_module=CbMod, display_module=DispMod,
                      n_reds=Reds, population=Tree, config=Config,
                      terminate=Terminate0} = _State) ->
    Fitness = CbMod:fitness(Instance, EvalResult),
    Target = proplists:get_value(fitness_target, Config, infinity),
    Terminate = Terminate0 orelse target_reached(Target, Fitness),
    ets:insert(Tree, {Fitness, Instance, EvalResult}),
    Size = ets:info(Tree, size),
    PopulationSize = proplists:get_value(population_size, Config, ?POPULATION_SIZE),
    if Size > PopulationSize -> ets:delete(Tree, ets:first(Tree));
       true -> ok
    end,
    if Terminate -> ok;
       Size >= PopulationSize -> spawn_work(Pid, PPid, CbMod, Size, Tree);
       true -> spawn_generate(Pid, PPid, CbMod)
    end,
    DisplayDecimator = proplists:get_value(display_decimator, Config, ?DISPLAY_DECIMATOR),
    if (Reds rem DisplayDecimator) == 0 -> update_status(CbMod, DispMod, Reds, Tree);
       true -> ok
    end.

target_reached(infinity, _Fitness) -> false;
target_reached(Target, Fitness) -> Fitness >= Target.

master() ->
    random:seed(now()),
    master_loop().

master_loop() ->
    receive
        {process_result, PPid, WorkResult, State} ->
            process_result(PPid, WorkResult, State),
            master_loop();
        stop ->
            ok
    end.

spawn_work(Pid, PPid, CbMod, Size, Tree) ->
    case utils:crandom([mutate, cross, gen]) of
        mutate ->
            BetterHalfKeys = utils:ets_keys(Tree, Size div 2, Size-1),
            %% Choose one randomly from the better half of the population
            K1 = utils:crandom(BetterHalfKeys),
            %% Do a tournament selection on half the population (favors better instances)
            %K1 = utils:tournament_select(BetterHalfKeys),
            [{K1,Inst1,_EvalResult}] = ets:lookup(Tree, K1),
            spawn_mutate(Pid, PPid, CbMod, Inst1);
        cross ->
            %% Keys = utils:ets_keys(Tree, 0, Size-1),
            %% K1 = utils:tournament_select(Keys, 5),
            %% K2 = utils:tournament_select(Keys, 5),
            %% [{K1,Inst1,_EvalResult1}] = ets:lookup(Tree, K1),
            %% [{K2,Inst2,_EvalResult2}] = ets:lookup(Tree, K2),
            Slots = lists:seq(0, Size-1),
            S1 = utils:tournament_select(Slots, 5),
            S2 = utils:tournament_select(Slots, 5),
            [{_K1,Inst1,_EvalResult1}] = ets:slot(Tree, S1),
            [{_K2,Inst2,_EvalResult2}] = ets:slot(Tree, S2),
            spawn_combine(Pid, PPid, CbMod, Inst1, Inst2);
        gen ->
            spawn_generate(Pid, PPid, CbMod)
    end.

spawn_generate(Pid, PPid, CbMod) ->
    Pid ! {run, fun() -> generate_eval(PPid, CbMod) end}.

spawn_mutate(Pid, PPid, CbMod, Inst1) ->
    Pid ! {run, fun() -> mutate_eval(PPid, CbMod, Inst1) end}.

spawn_combine(Pid, PPid, CbMod, Inst1, Inst2) ->
    Pid ! {run, fun() -> combine_eval(PPid, CbMod, Inst1, Inst2) end}.


save_worker_nodes(Workers) ->
    NodeList = [Node || {Node, _Cores, _Pids} <- Workers],
    NodesStr = io_lib:format("~p.", [NodeList]),
    file:write_file("digeno_workers.txt", NodesStr).


generate_eval(PPid, CbMod) ->
    Instance = CbMod:generate(),
    EvalResult = CbMod:evaluate(Instance),
    case CbMod:dead_on_arrival(Instance, EvalResult) of
        true -> generate_eval(PPid, CbMod);
        _    -> PPid ! #work_result{pid=self(), instance=Instance, eval_result=EvalResult}
    end.


mutate_eval(PPid, CbMod, Inst1) ->
    Instance = CbMod:mutate(Inst1),
    EvalResult = CbMod:evaluate(Instance),
    case CbMod:dead_on_arrival(Instance, EvalResult) of
        true -> mutate_eval(PPid, CbMod, Inst1);
        _    -> PPid ! #work_result{pid=self(), instance=Instance, eval_result=EvalResult}
    end.


combine_eval(PPid, CbMod, Inst1, Inst2) ->
    Instance = CbMod:combine(Inst1, Inst2),
    EvalResult = CbMod:evaluate(Instance),
    case CbMod:dead_on_arrival(Instance, EvalResult) of
        true -> combine_eval(PPid, CbMod, Inst1, Inst2);
        _    -> PPid ! #work_result{pid=self(), instance=Instance, eval_result=EvalResult}
    end.


update_status(CbMod, DispMod, Reds, Tree) ->
    %% %%Filename = io_lib:format("output/tree-~4..0B.bin", [Reds div 100]),
    %% Filename = "output/tree.bin",
    %% ok = filelib:ensure_dir(Filename),
    %% file:write_file(Filename, term_to_binary(Tree), [raw]);

    WorstFitness = ets:first(Tree),
    [{WorstFitness, WorstInst, WorstResult}] = ets:lookup(Tree, WorstFitness),
    BestFitness = ets:last(Tree),
    [{BestFitness, BestInst, BestResult}] = ets:lookup(Tree, BestFitness),

    DispMod:update_status(Reds, ets:info(Tree, size),
                          {CbMod:format(WorstInst), CbMod:format_result(WorstResult), WorstFitness},
                          {CbMod:format(BestInst), CbMod:format_result(BestResult), BestFitness}).
