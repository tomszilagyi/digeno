# DiGenO -- Distributed Genetic Optimizer

DiGenO is a Distributed Genetic Optimizer framework in Erlang. It
consists of a master node where all I/O happens, and any number of
worker nodes that perform parallelized computations of the genetic
algorithm.

You use DiGenO by writing a callback module that implements common
(genetic and utility) operations for your optimization problem. Then
you configure the master to load your callback module, and start
workers on any number of computational nodes.

DiGenO handles the transparent addition or deletion of computational
nodes, and the automatic distribution of operations to perform the
genetic algorithm. Computational nodes are ephemeral: you can shut
them down at any time or start up new ones all while the optimization
is running. You can even restart the master -- for example, after
changing your problem definition -- while the workers keep running and
wait for the master to reengage them.

## Algorithm

DiGenO implements a *steady state* evolution algorithm. By this we
mean that after an initial population is built up, subsequent
evolution is performed on a continuous basis, swapping out old
instances in the population for new ones piecewise. This is in
contrast with more conventional evolutional algorithms that iterate by
generating a complete successor population at once. They usually do
this by keeping a few percent of instances (the "elite") intact, and
filling up the rest by mutations and cross-overs of instances selected
from the preexisting pool.

Instead of this, DiGenO generates, evaluates and releases into the
population new instance candidates on a continuous basis, thereby
performing a more gradual evolution. Each occasion of releasing a
newly generated instance into the population is called a *reduction*.
The number of reductions is a metric showing how many steps the
genetic algorithm has done, and is similar to (but much larger than)
the count of successive populations evolved with a conventional
algorithm.

## Configuration

The file `digeno.config` contains options for DiGenO master. Currently
this consists of the callback module to use (this defines the
optimization problem) and the display callback module.

You can implement your own display module if you want (based on the
`digeno_display` behaviour) or just choose between the existing ones
(`src/display_*.erl`)

The callback module must be implemented as a `digeno_callback`
behaviour; please look at the file `src/digeno_callback.erl` for the
interface definition and study the provided examples.

## Running

Normally, you run the master node on the computer in front of you, and
one worker on each "supercomputer" in your basement. (The normal
provisions of Erlang clustering apply: it is assumed to run on a
trusted, fast, local network.)

`start_master.sh` starts the master node. `start_worker.sh` starts a
parallelized worker, which you can configure to use any number of
cores between 1 and the actual number of cores available. To do this,
set the shell environment variable `CORES` to a number. The default is
to use all available cores. You must also configure the worker so it
knows the Erlang nodename of the master node; set the environment
variable `DIGENO_MASTER` appropriately.

There is no point in running more than one worker on a machine.
However, you may or may not want to run a worker on the machine
running the master.

## Examples

Three example callback modules are provided. You can play with them,
and rip them off to base your own callback modules on them. They are
meant to demonstrate the usage of the `digeno_callback` behaviour via
a few toy problems.

Examples provided:
- example_string: Optimize randomly generated strings towards a given target
- example_curve: Fit a polynomial curve to a set of points
- example_tsp: Travelling Salesman Problem

The `utils` module contains several useful facilities for writing
functions that operate in a probabilistic manner (eg. use random
selection).

More on DiGenO: https://tomszilagyi.github.io/digeno
