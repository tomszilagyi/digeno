-module(digeno_callback).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').

%% Behaviour callbacks for the module encapsulating the GA optimisation problem

%% Return configuration to tune the GA
-type config_key() :: population_size    %% Size of the population to breed - integer
                    | fitness_target     %% Algorithm terminates when reached - float or infinity
                    | display_decimator. %% Number of reductions per status display - integer
-callback get_config() -> GAConfig :: [{config_key(), term()}].

%% Generate a new instance
-callback generate() -> Instance :: term().

%% Evaluate the fitness of an instance. Result must be a term
%% interpreted by callbacks `fitness' and `dead_on_arrival'.
%% TODO input user-defined computation context
-callback evaluate(Instance :: term()) -> EvalResult :: term().

%% Compute the fitness of a given instance based on its evaluation
%% result. This must return a floating point number that is greater
%% (more positive) for better instances.
-callback fitness(Instance :: term(), EvalResult :: term()) -> Fitness :: float().

%% It might be useful to discard certain instances right after generation
%% if they don't fulfill certain domain-specific criteria. Use this
%% callback to implement such filtering. Return true to block the instance
%% from entering the population.
-callback dead_on_arrival(Instance :: term(), EvalResult :: term()) -> true | false.

%% Mutate an instance.
-callback mutate(Instance :: term()) -> MutatedInstance :: term().

%% Combine two instances into a new one. Also called crossover in GA terms.
-callback combine(Instance1 :: term(), Instance2 :: term()) -> Instance :: term().

%% Format an instance into a human-readable string.
-callback format(Instance :: term()) -> string().

%% Format the result term into a human-readable string.
-callback format_result(EvalResult :: term()) -> string().
