-module(utils).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-export([ceil/1, floor/1,
         crandom/1, grandom/1, xrandom/3,
         drop_item/2, add_item/3, change_item/3,
         tournament_select/1, tournament_select/2,
         format_float/3,
         count_cores/0,
         round_robin/1,
         ets_keys/3]).
-export([randtest/1]).

%% numeric utilities
ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
	Pos when Pos > 0 -> T;
        _ -> T
    end.

%% choose one of the list members as random, with equal possibility
crandom(Choices) -> lists:nth(random:uniform(length(Choices)), Choices).

%% random integer in range [1..N], with decreasing probability
%% Test with: randtest(fun() -> grandom(20) end).
grandom(N) -> 1 + round((N-1) * math:pow(random:uniform(), 4)).

%% generate random value with rejection list
xrandom(RandFun, RandArgs, RejectList) ->
    R = apply(RandFun, RandArgs),
    case lists:member(R, RejectList) of
	true -> xrandom(RandFun, RandArgs, RejectList);
	_ -> R
    end.

randtest(RandFun) ->
    %% run grandom() a few times and display a histogram of result distributions:
    randtest(RandFun, [], 10000).

randtest(_RandFun, Dist, 0) ->
    lists:sort(fun({Val1, _Cnt1}, {Val2, _Cnt2}) -> Val1 =< Val2 end, Dist);
randtest(RandFun, Dist, N) ->
    Val = RandFun(),
    case lists:keyfind(Val, 1, Dist) of
	false -> randtest(RandFun, lists:keystore(Val, 1, Dist, {Val,1}), N-1);
	{Val, Count} -> randtest(RandFun, lists:keystore(Val, 1, Dist, {Val,Count+1}), N-1)
    end.

%% manipulate lists of items
drop_item(N, List) ->
    %% List item number N (one-based) removed from list
    lists:sublist(List, N-1) ++ lists:nthtail(N, List).

add_item(N, Item, List) ->
    %% Return a list with first N original items, then Item, then rest of the original items
    lists:sublist(List, N) ++ [Item] ++ lists:nthtail(N, List).

change_item(N, Item, List) ->
    %% List item number N (one-based) changed to Item
    lists:sublist(List, N-1) ++ [Item] ++ lists:nthtail(N, List).

%% classical tournament selection method
tournament_select(Keys) -> tournament_select(Keys, length(Keys)).

tournament_select(Keys, TournamentLength) ->
    ts_1(Keys, length(Keys), TournamentLength, []).

ts_1(_Keys, _N, 0, TKeys) ->
    ts_2(lists:sort(fun(K1, K2) -> K1 >= K2 end, TKeys)); % largest first!
ts_1(Keys, N, TL, TKeys) ->
    I = random:uniform(N),
    ts_1(Keys, N, TL-1, [lists:nth(I, Keys)|TKeys]).

-define(P, 0.5).

ts_2([TKz]) -> TKz;
ts_2([TK1|TKRest]) ->
    R = random:uniform(),
    if R < ?P -> TK1;
       true -> ts_2(TKRest)
    end.

%% safely format float into given number of digits
format_float(Value, Width, Digits) ->
    NCharsNeeded = if Value < 0.0 -> ceil(math:log10(-Value)) + Digits + 2;
		      Value > 0.0  -> ceil(math:log10(Value)) + Digits + 1;
		      true -> Digits + 1
		   end,
    if NCharsNeeded =< Width ->
	    io_lib:format("~*.*f", [Width, Digits, float(Value)]);
       NCharsNeeded - Digits + 1 =< Width -> % at least one digit can be printed
	    PrintDigits = Digits - (NCharsNeeded - Width),
	    io_lib:format("~*.*f", [Width, PrintDigits, float(Value)]);
       true -> % change to exp format
	    PrintDigits = if Value < 0.0 -> Width - 5;
			     true -> Width - 4
			  end,
	    io_lib:format("~*.*g", [Width, PrintDigits, float(Value)])
    end.

%% Determine the number of cores usable.
%% Return tuple: {Cores, MaxCores, LogicalProcs}
%%  - Cores: number of cores determined usable
%%  - MaxCores: limit on cores via UNIX env
%%  - LogicalProcs: Logical processors seen by BEAM
count_cores() ->
    LogicalProcs = erlang:system_info(logical_processors),
    MaxCores = case os:getenv("CORES") of
		   false -> infinity;
		   MCS -> list_to_integer(MCS)
	       end,
    Cores = case LogicalProcs of
		undefined ->
		    case MaxCores of
			infinity -> 1;
			MC -> MC
		    end;
		C -> min(C, MaxCores)
	    end,
    {Cores, MaxCores, LogicalProcs}.

%% Generate an infinite round-robin sequence based on the elements of
%% the list. This function is used to initialize the generator's state
%% with a list of elements to use in RR fashion.
round_robin(List) when is_list(List) -> {List, List};
%% Generate the next element in the RR sequence, and the new RR state.
%% Return {Element, NextState}.
round_robin({[], List}) -> round_robin({List, List});
round_robin({[E|Rest], List}) -> {E, {Rest, List}}.

%% Get list of keys from an ordered_set ets table.
%% Keys from positions Start to End are returned in Erlang term order.
ets_keys(Tab, Start, End) ->
    ets_keys(Tab, ets:info(Tab, keypos), Start, End, []).

ets_keys(Tab, Keypos, Start, Start, L) ->
    case ets:slot(Tab, Start) of
        [T] -> [element(Keypos, T) | L];
        _ -> L
    end;
ets_keys(Tab, Keypos, Start, End, L) ->
    case ets:slot(Tab, End) of
        [T] -> ets_keys(Tab, Keypos, Start, End-1, [element(Keypos, T) | L]);
        '$end_of_table' -> ets_keys(Tab, Keypos, Start, End-1, L)
    end.
