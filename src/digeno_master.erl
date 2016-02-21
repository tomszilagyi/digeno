-module(digeno_master).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(gen_server).
-export([start/2, start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% hardcoded default if missing from callback's get_config()
-define(DISPLAY_DECIMATOR, 100).
-define(POPULATION_SIZE, 100).

%% TODO allow multiple instances with the same fitness in the pool

-record(state, {callback_module, display_module, config, n_reds,
                population, workers, converg, terminate}).
-record(work_result, {pid, instance, eval_result, fitness}).

start(CbMod, DispMod) ->
    start_link(CbMod, DispMod).

start_link(CbMod, DispMod) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [CbMod, DispMod], []).

init([CbMod, DispMod]) ->
    ok = DispMod:init(CbMod),
    random:seed(now()),

    %% %% read back last saved state
    %% Filename = "output/tree.bin",
    %% {ok, TreeBin} = file:read_file(Filename),
    %% Tree = binary_to_term(TreeBin),
    Tree = gb_trees:empty(),

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

    Config = CbMod:get_config(),
    {ok, #state{callback_module=CbMod, display_module=DispMod,
                n_reds=0, population=Tree, config=Config, workers=[],
                converg=[], terminate=false}}.

handle_call(Request, From, State) ->
    io:format("~p: handle_call: req = ~p  from = ~p~n", [?MODULE, Request, From]),
    {reply, the_reply, State}.

handle_cast(Msg, State) ->
    io:format("~p: handle_cast: msg = ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info(#work_result{}=WorkResult, #state{}=State) -> %% process work result
    {noreply, process_result(WorkResult, State)};
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

process_result(#work_result{pid=Pid, instance=Instance, eval_result=EvalResult, fitness=Fitness},
               #state{callback_module=CbMod, display_module=DispMod,
                      n_reds=Reds, population=Tree0, config=Config,
                      converg=Converg0, terminate=Terminate0} = State) ->
    Target = proplists:get_value(fitness_target, Config, infinity),
    Converg = update_converg(Converg0, Reds, Fitness),
    Terminate = Terminate0 orelse target_reached(Target, Fitness) orelse converg_reached(Converg),
    Tree1 = gb_trees:enter(Fitness, {Instance, EvalResult}, Tree0),
    PopulationSize = proplists:get_value(population_size, Config, ?POPULATION_SIZE),
    Tree = crop_population(Tree1, gb_trees:size(Tree1) - PopulationSize),
    Size = gb_trees:size(Tree),
    PPid = self(),
    if Terminate -> ok;
       Size >= PopulationSize -> spawn_work(Pid, PPid, CbMod, Tree);
       true -> spawn_generate(Pid, PPid, CbMod)
    end,
    DisplayDecimator = proplists:get_value(display_decimator, Config, ?DISPLAY_DECIMATOR),
    DoDisplay = (Reds rem DisplayDecimator) == 0,
    if DoDisplay or Terminate -> update_status(CbMod, DispMod, Reds, Tree);
       true -> ok
    end,
    State#state{n_reds=Reds+1, population=Tree, converg=Converg, terminate=Terminate}.

target_reached(infinity, _Fitness) -> false;
target_reached(Target, Fitness) -> Fitness >= Target.

converg_reached([{R1, Fb} | _]) ->
    %% S1 = (Fw1 - Fw2) / (math:log(R1) - math:log(R2)),
    %% S2 = (Fw2 - Fw3) / (math:log(R2) - math:log(R3)),
    %% io:format("S1 = ~f  S2 = ~f~n", [S1, S2]),
    %% S1 < 0.1 * S2;
    false;
    %(Fw1 - Fw2) / (math:log(R1) - math:log(R2)) < 0.1 * (Fw2 - Fw3) / (math:log(R2) - math:log(R3));
converg_reached(_) -> false.

crop_population(Tree, N) when N > 0 ->
    {_,_,Tree1} = gb_trees:take_smallest(Tree),
    crop_population(Tree1, N-1);
crop_population(Tree, _) -> Tree.

spawn_work(Pid, PPid, CbMod, Tree) ->
    case utils:crandom([mutate, combine, gen]) of
        mutate ->
            %% Choose one randomly from the better half of the population
            K1 = utils:crandom(lists:nthtail(gb_trees:size(Tree) div 2, gb_trees:keys(Tree))),
            %% Do a tournament selection on half the population (favors better instances)
            %K1 = utils:tournament_select(gb_trees:keys(Tree), gb_trees:size(Tree) div 2),
            {Inst1,_EvalResult} = gb_trees:get(K1, Tree),
            spawn_mutate(Pid, PPid, CbMod, Inst1);
        combine ->
            Keys = gb_trees:keys(Tree),
            K1 = utils:tournament_select(Keys, 5),
            K2 = utils:tournament_select(Keys, 5),
            {Inst1,_EvalResult1} = gb_trees:get(K1, Tree),
            {Inst2,_EvalResult2} = gb_trees:get(K2, Tree),
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
    Fitness = CbMod:fitness(Instance, EvalResult),
    case CbMod:dead_on_arrival(Instance, EvalResult) of
        true -> generate_eval(PPid, CbMod);
        _    -> PPid ! #work_result{pid=self(), instance=Instance,
                                    eval_result=EvalResult, fitness=Fitness}
    end.


mutate_eval(PPid, CbMod, Inst1) ->
    Instance = CbMod:mutate(Inst1),
    EvalResult = CbMod:evaluate(Instance),
    Fitness = CbMod:fitness(Instance, EvalResult),
    case CbMod:dead_on_arrival(Instance, EvalResult) of
        true -> mutate_eval(PPid, CbMod, Inst1);
        _    -> PPid ! #work_result{pid=self(), instance=Instance,
                                    eval_result=EvalResult, fitness=Fitness}
    end.


combine_eval(PPid, CbMod, Inst1, Inst2) ->
    Instance = CbMod:combine(Inst1, Inst2),
    EvalResult = CbMod:evaluate(Instance),
    Fitness = CbMod:fitness(Instance, EvalResult),
    case CbMod:dead_on_arrival(Instance, EvalResult) of
        true -> combine_eval(PPid, CbMod, Inst1, Inst2);
        _    -> PPid ! #work_result{pid=self(), instance=Instance,
                                    eval_result=EvalResult, fitness=Fitness}
    end.


update_converg([], Reds, Fitness) ->
    [{Reds, Fitness}];
update_converg([{_, BestFitness} | _Rest] = Converg, _Reds, Fitness) when BestFitness >= Fitness ->
    Converg;
update_converg(Converg, Reds, Fitness) ->
    NewConverg = [{Reds, Fitness} | Converg],
    io:format("Converg: ~p~n", [NewConverg]),
    NewConverg. %lists:sublist(NewConverg, 10). %% limit length


update_status(CbMod, DispMod, Reds, Tree) ->
    %% %%Filename = io_lib:format("output/tree-~4..0B.bin", [Reds div 100]),
    %% Filename = "output/tree.bin",
    %% ok = filelib:ensure_dir(Filename),
    %% file:write_file(Filename, term_to_binary(Tree), [raw]);

    {WorstFitness, {WorstInst, WorstResult}} = gb_trees:smallest(Tree),
    {BestFitness, {BestInst, BestResult}} = gb_trees:largest(Tree),

    DispMod:update_status(Reds, gb_trees:size(Tree),
                          {CbMod:format(WorstInst), CbMod:format_result(WorstResult), WorstFitness},
                          {CbMod:format(BestInst), CbMod:format_result(BestResult), BestFitness}).
