# Reverse Turing Machine

An algorithm for running Turing Machines in reverse, such that inputs are generated rather than validated.

Repository contains:

-   Turing machine ADT

    -   [Machine](src/main/scala/turing/Machine.scala) (contains the machine rules)

    -   [Machine Tape](src/main/scala/turing/Tape.scala) (contains input/output symbols)

    -   [Machine State](src/main/scala/turing/MachineState.scala) (either `Accept`, `Reject` or a user-defined intermediate state)

    -   [Machine Configuration]((src/main/scala/turing/MachineConfiguration.scala)) (contains the machine rules, tape and state)

-   Algorithms

    -   [Forward execution](src/main/scala/turing/Machine.scala#L28) (i.e. validating inputs).

    -   [Reverse execution](src/main/scala/turing/Machine.scala#L34) (i.e. generating inputs).

-   Examples:

    -   [Type-1 Grammar](src/test/scala/turing/TuringMachineSpec.scala#L21) (i.e. context-sensitive)

    -   [Type-2 Grammar](src/test/scala/turing/TuringMachineSpec.scala#L33) (i.e. context-free)

    -   [Type-3 Grammar](src/test/scala/turing/TuringMachineSpec.scala#L41) (i.e. regular)

Note: these algorithms have been optimised, so please forgive the odd bit of non-FP code! (At one point the code was
being considered for a production setting.)

## What is this?

This repository is the result of a spike into Turing Machines.

The objective was to learn if total Turing machines (i.e. machines that always halt) can be run in reverse such that
the machines _generate_ their tapes rather than _interpret_ them.

That is: if a machine represents a predicate, and running it forwards validates if the tape conforms to that predicate,
then can we run the machine backwards such that it generates inputs that would satisfy said predicate?

The result was positive: Turing Machines can be run in reverse.
