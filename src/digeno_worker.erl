-module(digeno_worker).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(gen_server).

-export([start/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {workers}).

start() ->
    gen_server:start({global, node()}, ?MODULE, [config], []).

start_link() ->
    gen_server:start_link({global, node()}, ?MODULE, [config], []).

init([_Config]) ->
    group_leader(whereis(user), self()), %% do not redirect I/O to master node

    {Cores, MaxCores, LogicalProcs} = utils:count_cores(),
    io:format("~nDiGenO worker starting up:~n"),
    io:format("   Cores limited via UNIX env $CORES: ~p~n", [MaxCores]),
    io:format("   Logical processors seen by BEAM  : ~p~n", [LogicalProcs]),
    io:format("=> Cores reported to DiGenO master  : ~p~n~n", [Cores]),

    MasterNode = case os:getenv("MASTER_NODE") of
		     false -> die("Error: $MASTER_NODE not set in shell environment, exiting.~n");
		     MN -> list_to_atom(MN)
		 end,
    io:format("establishing connection with DiGenO master node: ~p~n", [MasterNode]),
    case net_kernel:connect_node(MasterNode) of
	false -> die("Error: unable to connect to master node, exiting.~n");
	true -> ok
    end,

    ok = global:sync(),
    io:format("connected to ~p~n", [MasterNode]),

    Workers = lists:map(fun(_) -> spawn(fun worker/0) end, lists:seq(1, Cores)),

    global:whereis_name(digeno_master) ! {node, node(), Cores, Workers},
    erlang:monitor_node(MasterNode, true),
    {ok, #state{workers=Workers}}.

handle_call(Request, From, State) ->
    io:format("~p: handle_call: req = ~p  from = ~p~n", [?MODULE, Request, From]),
    {reply, the_reply, State}.

handle_cast(Msg, State) ->
    io:format("~p: handle_cast: msg = ~p~n", [?MODULE, Msg]),
    {noreply, State}.

handle_info({nodedown, _Node}, #state{workers=Workers}=State) ->
    io:format("Master node down~n"),

    %% remove modules remote-loaded by master - next time master connects,
    %% there may be newer versions that should be loaded
    lists:foreach(fun({M, "DIGENO-REMOVE-ME"}) ->
			  code:purge(M),
			  code:delete(M);
		     ({_, _}) -> ok
		  end, code:all_loaded()),

    %% stop workers
    lists:foreach(fun(P) -> P ! stop end, Workers),
    {stop, normal, State#state{workers=[]}};
handle_info(Info, State) ->
    io:format("~p: handle_info: info = ~p~n", [?MODULE, Info]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("~p: terminate with Reason=~p~n", [?MODULE, Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("~p: code_change~n", [?MODULE]),
    {ok, State}.

worker() ->
    random:seed(now()),
    worker_loop().

worker_loop() ->
    receive
        {run, Fun} ->
            Fun(),
            worker_loop();
        stop ->
            ok
    end.

die(Str) ->
    io:format(Str),
    init:stop(),
    receive after 10000 -> ok end.
