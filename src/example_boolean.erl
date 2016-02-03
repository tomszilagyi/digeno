-module(example_boolean).
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

% debug
-compile([export_all]).

%% This is a module defining a Genetic Optimisation problem.

%% Boolean logic function:
%%
%% - technology: list of logic functions (and2, or2, xor3, nand2, nor2 etc)
%% - an instance is a composite function built from the functions allowed
%%   by the technology
%% - multi-bit width input and output is handled as a set of functions all
%%   dependent on the inputs, evaluated together
%% - the function is evaluated on a randomly chosen set of N input
%%   vectors, and the squared Hamming difference of the outputs from the
%%   correct output is taken
%% - as the fitness increases, N can be increased as well (don't start with
%%   the full width, exhaustive testing would be very expensive).
%%
%% Let's say we want to make an 8-bit adder. This means there are 16 input
%% bits and 8 output bits, with an obvious mapping from all the possible
%% input combinations to the correct output. This is the function we are
%% optimizing for, that we want to create in (the simplest possible) closed
%% form.
%%
%% The functions we want are:
%% - y0(x0, x1, x2, ..., x15)
%% - y1(x0, x1, x2, ..., x15)
%%   ...
%% - y7(x0, x1, x2, ..., x15)

%% Representing logic functions as data:
%% We go with the traditional Lisp way, so eg.
%%   [xor2 x0 [xor2 x1 x2]]
%% is a possible representation of the xor3 function.

%% Bindings of input variables are represented with a list of all
%% variables bound to true. The value of any variable is thus equivalent
%% with its membership in the bindings list.

%% Implementation of elemental boolean functions

not1(X0) -> not X0.
or2(X0, X1) -> X0 or X1.
and2(X0, X1) -> X0 and X1.

technology() ->
    %%  id  arity eval fun
    [{not1, 1,    fun not1/1},
     { or2, 2,    fun or2/2},
     {and2, 2,    fun and2/2}].

input(N) -> list_to_atom("x" ++ integer_to_list(N)).

random_input(InputArity) ->
    N = random:uniform(InputArity) - 1,
    input(N).

random_function(InputArity) ->
    case utils:grandom(3) of
        1 -> random_input(InputArity);
        _ -> {F, A, _Fun} = utils:crandom(technology()),
             Args = [random_function(InputArity) || _S <- lists:seq(1, A)],
             [F | Args]
    end.

eval_bool(Sym, Bind) when is_atom(Sym) -> lists:member(Sym, Bind);
eval_bool([Func | Args], Bind) ->
    EvalArgs = [eval_bool(A, Bind) || A <- Args],
    {_, _, Fun} = lists:keyfind(Func, 1, technology()),
    apply(Fun, EvalArgs).

%% bit width needed to represent N
n_bits(0) -> 1;
n_bits(N) -> utils:ceil(math:log(N) / math:log(2)).

%% N-th row of truth table -- input as binding
input2binding(N) ->
    [input(I) || I <- lists:seq(0, n_bits(N)), N band (1 bsl I) /= 0].

%% N-th row of truth table -- desired output
output(N) ->
    %% example: xor3 function
    length(input2binding(N)) rem 2 =:= 1.

%% evaluate an instance -- error summing
eval_error(Results) -> eval_error(Results, 0).
%eval_error(Results) -> Results.

eval_error([], R) -> R;
eval_error([{X,X} | Rest], R) -> eval_error(Rest, R);
eval_error([_|Rest], R) -> eval_error(Rest, R+1).

eval_complexity(Sym) when is_atom(Sym) -> 0;
eval_complexity([_Func | Args]) -> lists:sum([1 | [eval_complexity(A) || A <- Args]]).

-define(ARITY, 3).

eval_instance(Instance) ->
    eval_error([{output(N), eval_bool(Instance, input2binding(N))}
                || N <- lists:seq(0, 1 bsl ?ARITY - 1)]) bsl 6
        + eval_complexity(Instance).

%% digeno callbacks

get_config() -> [{population_size, 100},
                 {fitness_target, 1.0},
                 {display_decimator, 100}].

generate() -> random_function(?ARITY).

mutate(Sym) when is_atom(Sym) -> random_input(?ARITY);
mutate([Func | Args]) ->
    Pos = random:uniform(length(Args)),
    [Func | utils:change_item(Pos, random_function(?ARITY), Args)].

combine(I1, _I2) ->
    %% TODO
    I1.

evaluate(Instance) -> eval_instance(Instance).

dead_on_arrival(_Instance, _EvalResult) -> false.

fitness(_Instance, 0) -> 2.0;
fitness(_Instance, EvalResult) -> 1.0 / EvalResult.

format(Instance) -> io_lib:format("~p", [Instance]).

format_result(EvalResult) -> io_lib:format("~B", [EvalResult]).

%% private functions

