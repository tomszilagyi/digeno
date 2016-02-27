-module(example_string).
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

-define(TARGET, "Now is the time for all good men to come to the aid of their party!").

%% digeno callbacks

get_config() -> [{population_size, 100},
                 {fitness_target, 2.0},
                 {display_decimator, 100}].

generate() -> gen_random_string([], length(?TARGET)).

mutate(Instance) ->
    Pos = random:uniform(length(Instance)),
    utils:change_item(Pos, lists:nth(Pos, Instance) + utils:crandom([1, -1]), Instance).

combine(I1, I2) ->
    Pos = random:uniform(length(I1)),
    I1a = lists:sublist(I1, Pos),
    I1b = lists:nthtail(Pos, I1),
    I2a = lists:sublist(I2, Pos),
    I2b = lists:nthtail(Pos, I2),
    utils:crandom([I1a ++ I2b, I2a ++ I1b]).

evaluate(Instance) -> distance(Instance, ?TARGET).

dead_on_arrival(Instance, _EvalResult) -> length(Instance) /= length(?TARGET).

fitness(_Instance, 0) -> 2.0;
fitness(_Instance, EvalResult) -> 1.0 / EvalResult.

format(Instance) -> Instance.

format_result(EvalResult) -> io_lib:format("~B", [EvalResult]).

%% private functions

random_char() -> $  + utils:grandom(95). %% ascii printable

gen_random_string(Str, 0)   -> Str;
gen_random_string(Str, Len) -> gen_random_string([random_char() | Str], Len-1).

dif({A, A}) -> 0;
dif({A, B}) -> (A - B) * (A - B).

distance(S1, S2) -> lists:sum(lists:map(fun dif/1, lists:zip(S1, S2))).
