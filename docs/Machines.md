# Overview of Kiama's support for abstract state machines

Up: [User Manual](UserManual), Prev: [Relations](Relations), Next: [Pretty-printing](PrettyPrinting)

IMPORTANT NOTE: This page describes Kiama 1.x. Much of it also applies
to Kiama 2.x, but not all. Please consult the 2.x release notes for the
main differences. We are currently writing comprehensive documentation
for 2.x that will eventually replace these pages.

This page provides an overview of Kiama's support for abstract state
machines. Abstract state machines are used in the following examples.

**ISWIM** RISC

## Introduction

The dynamic semantics of programming languages are often defined in
terms of formal machines. These machines are defined by instruction
sets that specify the operations that the machine can perform, a
machine state that describes the information that is maintained as the
machine executes, and evaluation rules that specify how each type of
instruction affects the machine state.

For example, languages such as Java and C# are executed on abstract
machines implemented by the Java Virtual Machine or the .Net runtime.
Functional programming languages are often defined by machines that
perform stepwise reduction of function applications or more complex
processes such as graph reduction for lazy evaluation.

Kiama's _abstract state machine_ library provides a way to specify
these kinds of machines in a high-level way.
[Abstract state machines](http://en.wikipedia.org/wiki/Abstract_state_machines)
were originally developed to provide a formal notation for
generalisations of finite state machines. The idea is that instead of
the state of the machine being a single discrete value, it can
comprise multiple values of arbitrary structured data.

Kiama's _abstract state machine_ library allows you to implement these
kinds of machines in a high-level way. Abstract state machines were
originally developed to provide a theory for generalisations of
finite-state machines. In a finite-state machine the states are single
discrete atomic values. In abstract state machines the states comprise
multiple _state items_, each of which can be a structured value.

Abstract state machines have been used to formally specify many
dynamic processes and their refinement to implementations. In Kiama we
are primarily interested in using them to implement abstract machines
in a way that closely matches their formal specification, particularly
for language prototyping.

Kiama provides a form of abstract state machine that is quite similar
to those seen in formal treatments. The rest of this page describes
how to write abstract state machines in Kiama. The running example is
the simple RISC machine from the RISC example, which is
based on the machine described in Wirth's book
[Compiler Construction](http://www.cs.inf.ethz.ch/~wirth/books/CompilerConstruction/).

## Abstract state machines in Kiama

The base class of a Kiama abstract machine is `org.bitbucket.inkytonik.kiama.machine.Machine`.

```
package org.bitbucket.inkytonik.kiama.machine

abstract class Machine (val name : String, emitter : Emitter = new Emitter)
```

The `name` argument is used to identify the machine in trace messages.
The `emitter` is used to produce output and defaults to standard
output.

To define a new machine, you inherit from `Machine`. In the RISC
example, the machine is implemented by the `RISC` class which is
constructed from the code sequence that is to be run, a console from
which input should be read and an emitter to which output should be
sent.

```
import org.bitbucket.inkytonik.kiama.machine.Machine
import org.bitbucket.inkytonik.kiama.util.Console
import org.bitbucket.inkytonik.kiama.util.Emitter

class RISC (code : Code, console : Console, emitter : Emitter)
       extends Machine ("RISC", emitter)
```

## RISC code sequences

The `Code` type is a sequence of machine instructions. The actual
instructions are essentially as defined by Wirth and are typical of
RISC machines. They are defined here by case classes in the usual
Scala way. We show a few typical examples; see the code for a complete
list.

```
type Code = Seq[Instr]

type RegNo = Byte

abstract class Instr

case class MOV (a : RegNo, b : Int, c : RegNo) extends Instr
case class ADD (a : RegNo, b : RegNo, c : RegNo) extends Instr
case class CMPI (b : RegNo, im : Int) extends Instr
case class LDW (a : RegNo, b : RegNo, im : Int) extends Instr
case class BEQ (val label : Int) extends Branch
case class RET (c : RegNo) extends Instr
```

`MOV` is a move from register `c` to register `a` with a shift `c`.
`ADD` is a register to register addition. `CMPI` sets condition codes
based on a comparison between a register value and an immediate value.
`LDW` is a load from a memory location to register `a`; the memory
address is given by the second and third arguments. `BEQ` performs a
displacement branch if the previous comparison indicated that the two
compared values were equal. Finally, `RET` returns from a sub-routine
call using the return address stored in the given register, or halts
the program if it is the main subroutine that is returning.

## Abstract state machine basics

The state of an abstract state machine comprises distinct named
structured values that we call _state items_. Each state item is
initially _undefined_ and may become defined as the machine executes.

The state items are updated by the _transition rules_ of the machine.
The effect of executing the transition rules is that zero or more
state items are updated.

A crucial property of abstract state machines is that the updates to
state items made in one step are not visible until the next execution
step. In other words, the updates happen simultaneously at the end of
the step. If a particular state item is updated more than once by a
single step, the updates are required to be _consistent_, in that they
must all update the state item to the same value. The machine will
abort if inconsistent updates are performed.

Execution continues stepwise in this fashion until a step in which no
updates are performed, at which time the machine halts. An update that
happens to not change the value of a state item is still an update as
far as machine termination goes.

## RISC machine state

In the following we will define an abstract machine to execute RISC
machine code sequences. The main state items of the machine are a
register file called `R` and a memory called `Mem`.

The register file is indexed by the number of the register. We assume
32 registers, so the indexes will be 0 to 31. Most of the registers
are used for general purposes, but some are special: 28 is the program
counter, 29 is the frame pointer, 30 is stack pointer and 31 is the
link register that holds return addresses during subroutine calls.

The memory consists of words addressed by integer addresses and is
assumed to be arbitrarily large for the purposes of this example.

As well as the registers and memory, the machine has two condition
codes `Z` and `N`, which are set by the comparison instructions and
tested by the conditional branches. `Z` is set if two compared values
are equal, while `N` is set if the first value is strictly less than
the second value.

## Simple state items

The simplest state items of an abstract machine are values of type
`State[T]`, where `T` is the type of the underlying value. The `State`
constructor has a parameter `sname` which is used to identify the
state item in trace messages.

```
class State[T] (sname : String)
```

We can therefore define the `Z` and `N` state items as follows.

```
val Z = new State[Boolean] ("Z")
val N = new State[Boolean] ("N")
```

The machine also has a simple Boolean state item `halt` that indicates
whether it should keep executing or not.

```
val halt = new State[Boolean] ("halt")
```

## Parameterised state items

Simple state items are sufficient for many purposes, but it is common
to want parameterised state where many state values are collectively
named and indexed by a parameter. Kiama provides `ParamState` to
implement parameterised state items.

```
class ParamState[T,U] (val sname : String) extends State[HashMap[T,U]] (sname)
```

A parameterised state item is implemented as a simple state item whose
value is a map.

In the RISC machine, the register file is parameterised by the
register number.

```
val R = new ParamState[RegNo,Int] ("R")
```

Aliases are declared for the special purpose registers such as the
program counter (`PC`) and the link register used for return addresses
in subroutine calls (`LNK`). These definitions use a function call
notation to access a particular component of the parameterised state
item `R`.

```
val PC = R (28)
val FP = R (29)
val SP = R (30)
val LNK = R (31)
```

The memory of the RISC machine is also represented by a parameterised
state item, where the parameter in this case is the memory address.

```
val Mem = new ParamState[Int,Int] ("Mem")
```

## Initialisation

Initialisation of state is performed by the `init` transition rule.
`Machine` provides a default definition that does nothing. Most
machines override this definition to properly initialise their state
items. We say that uninitialised state items are _undefined_ and any
attempt to use their values causes the machine to abort.

Transition rules are implemented by regular Scala functions or
methods, using a special assignment notation to indicate state
updates. Here is the initialisation for the RISC machine that sets the
program counter to the beginning of the code sequence, ensures that
register zero is zero, clears the condition codes, and sets the halt
flag to false.

```
override def init {
   PC := 0
   R (0) := 0
   Z := false
   N := false
   halt := false
}
```

When a rule wants to record a state update for the current step, it
uses the `:=` operator instead of a normal Scala assignment statement.
The left-hand side of an update can be a simple state item or a
parameterised state item indexed by an appropriate value. The
right-hand side of an update can be any expression with a value of the
appropriate type.

As discussed above, the recorded updates do not actually happen until
the end of the current step. All uses of a state item within a single
step will see the same value. The updates are recorded for consistency
checking at the end of the step and, if the updates are consistent,
the affected state items will be updated at that point before the next
step begins.

To preserve the formal properties of a machine, all state changes that
affect the operation of the transition rules should be performed via
updates of `State` or `ParamState` values, rather than by side-effects
on Scala variables.

## The main transition rule

Every abstract machine must define a `main` transition rule. It is
called once for each step of the machine. `main` can perform state
updates itself, or more usually, it calls other functions or methods
that implement different aspects of the machine's semantics.

In the RISC machine the main rule is

```
def main {
   if (!halt)
       execute (code (PC))
}
```

This rule checks that the machine is not supposed to halt, and if so,
runs the `execute` rule on the instruction at the current program
counter location. Recall that `PC` is the alias for the part of the
register state associated with index 28. The state implementation is
hidden, so that the value can be accessed as if `PC` was just a
regular integer value.

## Executing instructions

The `execute` rule takes the current instruction as a parameter and
calls other rules that handle each of the different classes of machine
instruction.

```
def execute (instr : Instr) {
   arithmetic (instr)
   memory (instr)
   control (instr)
   inputoutput (instr)
}
```

As defined here, `execute` does not care which of the other functions
actually handles the instruction. In fact, zero or more of the
functions may do so, depending on how the machine semantics has been
decomposed. All that matters is that the state updates performed by
the functions are consistent.

In the decomposition used in the example, each of the rules
`arithmetic`, `memory`, `control` and `inputoutput` deals with a
subset of the instruction types and ignores the other types. Since
every side-effect that can affect the operation of the machine is
performed using a deferred update on a state item, the order in which
the functions are called does not matter.

To execute arithmetic instructions, the `arithmetic` rule matches on
the instruction type and updates the appropriate state. We show the
cases for `MOV`, `ADDI` and `CMPI`.

```
def arithmetic (instr : Instr) {
   instr match {
       case MOV (a, b, c)   => R (a) := R (c) << b
       case ADD (a, b, c)   => R (a) := R (b) + R (c)
       case CMPI (b, im)    => Z := R (b) =:= im
                               N := R (b) < im
       ...
       case _               =>
   }
```

Note again the use of the `:=` operator to record updates. The
right-hand sides are more complex than seen earlier in the `init`
rule. In almost all cases, standard Scala operators can be used to
calculate with state item values. A notable exception is equality
comparison which should be performed using the `=:=` operator, which
compares the underlying state values, instead of the normal `==`,
which compares the state items themselves.

Here are some more cases from the rules to illustrate how other
typical instructions are handled. From `memory`:

```
case LDW (a, b, im) => R (a) := Mem ((R (b) + im) / 4)
```

From `control`:

```
case b : BEQ if (Z) => PC := PC + b.disp
case RET (c)        => PC := R (c)
                       if (R (c) =:= 0) halt := true
```

Note how the `halt` state item is set to true here if the return
address is zero, indicating that the main subroutine is returning.
This change will be detected by the `main` rule.

## Other aspects

The basic functionality shown above is sufficient to define
deterministic abstract state machines. We plan to add some support for
non-determinism in a future version of the library.

Normally updates are performed automatically by the machine at the end
of the `init` rule and then at the end of each step. Sometimes it is
useful to be able to force updates to happen immediately. Call the
`performUpdates` method to get this effect. One use case for
performing updates immediately is when the `init` rule needs to
initialise some state items using the values of other state items. The
latter items should be initialised first, then `performUpdates`
called, then the dependent state items should be initialised.

## Debugging

You can set the `debug` flag of a machine to true to obtain a trace of
steps and state updates as the machine runs.

```
override def debug = true
```

Here is beginning part of the debug trace for the RISC machine showing
the initialisation and the first three steps of executing a program.
The library prints the state updates and step indicators, while the
machine itself prints the executed instructions.

```
RISC.R(28) := 0
RISC.R(0) := 0
RISC.Z := false
RISC.N := false
RISC.halt := false
RISC step 0
RISC exec: ADDI(29,0,0)
RISC.R(29) := 0
RISC.R(28) := 1
RISC step 1
RISC exec: ADDI(30,0,12)
RISC.R(30) := 12
RISC.R(28) := 2
RISC step 2
RISC exec: RD(1)
RISC.R(28) := 3
```

Up: [User Manual](UserManual), Prev: [Attribution](Attribution), Next: [Pretty-printing](PrettyPrinting)
