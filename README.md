# Leader election

This is an implementation of a unidirectional ring [leader
election](https://en.wikipedia.org/wiki/Leader_election) algorithm in
Erlang.

We use a controling process that launch all nodes and connect them with a given
topology (ring, complete or random). The controler can then broadcast values to
all node or communicate with individual nodes, e.g. to launch a distributed
algorithm.  This idea can be easily adapted to other message-passing algorithms.

To run the lcr algorithm start by compiling the erlang files with "make".

then you can either:

1. run the script `lcr N` where `N` is the number of processes in the ring

Remark: you must stop the execution with "CTRL-C".

2. launch the erl command line interpreter and copy the following lines.

        N = 10.
        {S, B} = control : init(lcr, N, topology : ring(N), fun(X) -> random : uniform(1000) end).
        B(start).

`control : init` initializes `N` processes that run the `lcr : proc` function
in a unidirectional ring topology. Each process is initialized with some
state (parameter 4) via a function that takes a process number as parameter
(unused here). For the LCR algorithm, it generates a (random) UIN (we could
use `X`to make sure that there can't be any collision - cf. lcr script).

`control : init` returns a pair of functions `S` and `B`. `S(P,V)` sends a
value `V` to process `P` (identified by a number between `1` and `N`). And
`B(V)` broadcasts a value `V` to all processes.

To run the algorithm, we broadcast atomic value `start` to all processes with
`B(start)`.

We can run the algorithm again, or terminate the processes with `B(quit)`.

# Tendermint

WIP