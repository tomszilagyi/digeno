-module(digeno_display).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').

%% Behaviour callbacks for the module implementing the display / UI

%% Initialize the display module
-callback init(CbMod :: atom(),
               Cores :: pos_integer()) ->
    ok | {error, Reason :: term()}.

%% Update the displayed list of worker nodes
-callback update_workers(WorkerNodes :: [{Node :: atom(),
                                          Cores :: pos_integer(),
                                          Pids :: [pid()]}]) -> ok.

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
