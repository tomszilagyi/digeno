{application, digeno,
 [{description, "DiGenO -- Distributed Genetic Optimizer"},
  {vsn, "1.0.0"},
  {modules, [digeno_callback,
             digeno_display,
	     digeno_master,
	     digeno_supervisor,
	     digeno_worker,
	     utils]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {digeno_supervisor, []}},
  {start_phases, []}
 ]}.
