-module(digeno_display).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').

%% Behaviour callbacks for the module implementing the display / UI

%% Initialize the display module
-callback init(CbMod :: atom()) -> ok | {error, Reason :: term()}.

%% Update the displayed list of worker nodes
%% Called when the worker node list has changed.
-callback update_workers(WorkerNodes :: [{Node :: atom(),
                                          Cores :: pos_integer(),
                                          Pids :: [pid()]}]) -> ok.

%% Update convergence status
%% Called when there is a new best fitness
-callback update_converg(Reductions::pos_integer(),
                         BestFitness :: float()) -> ok.

%% Update the GA status display. Note that this callback gets the
%% instance and result already converted to printable (string) form.
-callback update_status(Reductions :: non_neg_integer(),
                        PopulationSize :: non_neg_integer(),
                        {WorstInst :: string(),
                         WorstResult :: string(),
                         WorstFitness :: float()},
                        {BestInst :: string(),
                         BestResult :: string(),
                         BestFitness :: float()}) -> ok.
