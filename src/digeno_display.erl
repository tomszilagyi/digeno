-module(digeno_display).
-author('Tom Szilagyi <tomszilagyi@gmail.com>').

%% Behaviour callbacks for the module implementing the display / UI

%% The display module is stateful: it returns an initial state on
%% successful init, and gets the actual state on each subsequent
%% call. These calls return a potentially updated state. The state is
%% opaque; it is never interpreted, only passed around by DiGenO.

%% Initialize the display module
-callback init(CbMod :: atom()) -> {ok, State :: term()} |
                                   {error, Reason :: term()}.

%% Update the displayed list of worker nodes
%% Called when the worker node list has changed.
-callback update_workers(WorkerNodes :: [{Node :: atom(),
                                          Cores :: pos_integer(),
                                          Pids :: [pid()]}],
                         State :: term()) -> NewState :: term().

%% Update convergence status
%% Called when there is a new best fitness
-callback update_converg(Reductions::pos_integer(),
                         BestFitness :: float(),
                         State :: term()) -> NewState :: term().

%% Update the GA status display. Note that this callback gets the
%% instance and result already converted to printable (string) form.
-callback update_status(Reductions :: non_neg_integer(),
                        PopulationSize :: non_neg_integer(),
                        {WorstInst :: string(),
                         WorstResult :: string(),
                         WorstFitness :: float()},
                        {BestInst :: string(),
                         BestResult :: string(),
                         BestFitness :: float()},
                        State :: term()) -> NewState :: term().
