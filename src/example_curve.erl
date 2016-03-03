-module(example_curve).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').
-behaviour(digeno_callback).

%% digeno callback functions
-export([get_config/0,
         generate/0,
         evaluate/1,
         fitness/2,
         dead_on_arrival/2,
         mutate/1,
         combine/2,
         format/1,
         format_result/1]).

%% This is a module defining a Genetic Optimisation problem.

%% Problem of curve fitting a polynomial function against data points:
%% Given an integer N and a set of data points [{X, Y}], determine the
%% coefficients of a univariate polynomial function of degree N that
%% best fits the set of data. Let the error metric to be minimized be
%% the sum of squared differences.

%% Instances are polynomial functions of the form:
%%   y(x) = a0 + a1 x + a2 x^2 + ... + an x^n

%% Instance representation: a list of coefficients [a0, a1, ..., an]

%% Example problem: sin(x) in the range [-pi, pi].
%%
%% The best possible instance is approximately:
%%   [0, 1, 0, -0.166666, 0, 0.008333, 0, -0.000198]
%% according to the Taylor series expansion:
%%   [0, 1, 0, -1/(3!), 0 1/(5!), 0, -1/(7!), ...]

-define(DEGREE, 7).

%% digeno callbacks

get_config() -> [{population_size, 1000},
                 {fitness_target, 1000.0},
                 {converg_detect, auto},
                 {display_decimator, 1000}].

generate() -> random_poly(?DEGREE).

mutate(Fn) -> mutate(Fn, utils:grandom(?DEGREE)).

combine(Fn1, Fn2) ->
    [utils:crandom([Ai, Bi]) || {Ai, Bi} <- lists:zip(Fn1, Fn2)].
%% Test: combine([a0, a1, a2, a3, a4, a5, a6, a7],
%%               [b0, b1, b2, b3, b4, b5, b6, b7]).

evaluate(Fn) -> eval_error(Fn, data()).

dead_on_arrival(_Fn, _Result) -> false.

fitness(_Fn, 0) -> 2.0;
fitness(_Fn, Result) -> 1.0 / Result.

format(Fn) -> string:join([io_lib:format("~9.6f", [Ai]) || Ai <- Fn], " ").

format_result(Result) -> io_lib:format("~f", [Result]).

%% private functions

mk_data(Fun, {StartX, EndX}, NumPoints) ->
    Delta = (EndX - StartX) / (NumPoints - 1.0),
    mk_data(Fun, StartX, Delta, NumPoints-1, []).

mk_data(Fun, StartX, _Delta, 0, Acc) ->
    X = StartX,
    [{X, Fun(X)} | Acc];
mk_data(Fun, StartX, Delta, NumPoints, Acc) ->
    X = StartX + NumPoints * Delta,
    mk_data(Fun, StartX, Delta, NumPoints-1, [{X, Fun(X)} | Acc]).

data() -> mk_data(fun(X) -> math:sin(X) end, {-math:pi(), math:pi()}, 100).

eval_fn(Fn, X) ->
    {Y, _} = lists:foldl(fun(Ai, {Acc, Xi}) ->
                                 {Acc + Ai * Xi, X * Xi}
                         end, {0.0, 1.0}, Fn),
    Y.

eval_error(Fn, Data) ->
    Errors = [eval_fn(Fn, X) - Y || {X, Y} <- Data],
    lists:sum([E*E || E <- Errors]).

random_coeff() -> utils:crandom([-1, 1]) * random:uniform().

random_poly(N) -> [random_coeff() || _P <- lists:seq(0, N)].

adjust_coeff(A) ->
    case utils:crandom([nudge1, nudge2, mag, inv, sign, set0, set1, rand]) of
        nudge1 -> A * utils:crandom([0.2, 0.3, 0.5, 0.7, 1.4, 2.0, 3.0, 5.0]);
        nudge2 -> A * utils:crandom([0.9, 0.99, 0.999, 0.9999, 1.1, 1.01, 1.001, 1.0001]);
        mag    -> A * utils:crandom([0.0001, 0.001, 0.01, 0.1, 10, 100, 1000, 10000]);
        inv when A /= 0.0 -> 1.0 / A;
        inv    -> 0.0;
        sign   -> -A;
        set0   -> 0.0;
        set1   -> 1.0;
        rand   -> random_coeff()
    end.

%% mutate N times in a row to allow changing different coefficients at once
mutate(Fn, 0) -> Fn;
mutate(Fn, N) ->
    I = random:uniform(length(Fn)),
    Ai = lists:nth(I, Fn),
    NewAi = adjust_coeff(Ai),
    mutate(utils:change_item(I, NewAi, Fn), N-1).
