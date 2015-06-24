# Profiling Kiama-based programs

Up: [User Manual](UserManual.md), Prev: [Read-eval-print Loops](ReadEvalPrintLoops.md)

This page briefly describes Kiama's support of profiling the execution of
Kiama-based programs.

For more detailed descriptions of the motivations behind this profiling
support and how it works, please see our SLE 2012 paper on the
[Kiama research wiki page](Research.md).

Please consult the documentation for our `dsprofile` library to find out
more about the implementation of profiling for internal domain-specific
languages.

## Introduction

As of version 1.5.0 Kiama includes support for profiling the execution
of Kiama-based programs.
Specifically, Kiama can now record the execution of attribute computations
and rewriting transformations.
After execution is finished you can request a report that summarises the
execution along various profiling _dimensions_.

For example, you can find out how many times an attribute was evaluated and
at which nodes,
how much execution time those evaluations took, what values were
computed and whether those values came from attribute caches or not.
Similarly, for rewriting you can find out which strategies were applied
to which nodes, how much time was consumed, and what the results of those
applications were.

The rest of this page illustrates Kiama's profiling support using the
PicoJava example.

## Profiling the PicoJava example

The PicoJava example uses the `ProfilingCompiler` trait from Kiama's
utility module.
This trait provides command-line handling to let you specify options
that request a profile report and the dimensions that should appear
in that report.

For example, you can run with the `-p` option to specify the dimensions you
wish to see.

```
test:run-main org.kiama.example.picojava.Main -pevent -o src/org/kiama/example/picojava/tests/ObfuscationTest.pj
```

In this run we request the `event` dimension.
We also pass the `-o` option to ask the program to perform an obfuscation
transformation on the input program so that it will do some rewriting.
As well as the normal output of the program, you should see a profile like
this:

```
   187 ms total time
    57 ms profiled time (30.6%)
  1121 profile records

By event:

 Total Total  Self  Self  Desc  Desc Count Count
    ms     %    ms     %    ms     %           %
    34  59.3    33  59.1     0   0.3   874  78.0  StratEval
    23  40.9    23  40.9     0   0.0   247  22.0  AttrEval
```

The header shows summary information about the execution: the total time
that it took, the amount of that time that was accounted for by profiled
events and the number of profiling records that were obtained.
The latter corresponds to the number of interesting runtime events such
as rewriting strategy applications or attribute evaluations.

The `event` dimension tells us the kind of evaluation that a profiling
record represents.
Thus, we see that this execution performed 874 strategy applications and
247 attribute evaluations.
The Total columns show the total time for each evaluation and all of the
ones that it used.
The Self and Desc (descendants) columns split the total time into the time
consumed by the evaluation itself and the time consumed by the other
evaluations that that evaluation used.
E.g. if you evaluate an attribute and the computation of that attribute
requires the values of other attributes then the time to compute those
other attributes will be included in the descendant portion.

## Multiple profiling dimensions

If you specify more than one comma-separated dimension in the `-p` option
you will get more than one table in the report.
The first table summarises along the first dimension.
The next series of tables summarise along the second dimension for each row
in the first table.
(This pattern continues if more than two dimensions are requested.)

For example, the `name` dimension displays the name of the strategy or
attribute that was used in a particular evaluation, so running the
PicoJava example with `-pevent,name` gives tables like the following
as well as the top-level table as shown above.

```
By name for StratEval:

 Total Total  Self  Self  Desc  Desc Count Count
    ms     %    ms     %    ms     %           %
    32  57.2     0   0.3    32  56.9     1   0.1  obfuscateProgram
    30  54.4     8  15.1    22  39.4   150  13.4  all
    18  32.8     3   7.0    14  25.8    75   6.7  obfuscateUses
    15  27.2     7  13.5     7  13.7   150  13.4  attempt
    13  24.6     4   8.6     9  16.0    75   6.7  obfuscateDecls
     6  10.8     3   6.7     2   4.1    75   6.7  <+
     1   3.2     1   3.0     0   0.3    71   6.3  obfuscateNormalUse
     1   2.0     1   2.0     0   0.0    75   6.7  obfuscateDecl
     1   1.8     1   1.8     0   0.0   127  11.3  id
     0   1.1     0   1.1     0   0.0    75   6.7  preservePredefinedUse

By name for AttrEval:

 Total Total  Self  Self  Desc  Desc Count Count
    ms     %    ms     %    ms     %           %
    19  35.0     1   2.7    18  32.3    38   3.4  decl
    18  32.4     6  11.1    12  21.2    42   3.7  lookup
    10  18.7     8  14.6     2   4.2    23   2.1  localLookup
     3   5.5     0   1.2     2   4.3    13   1.2  unknownDecl
     1   3.5     1   3.2     0   0.2    22   2.0  tipe
     1   3.0     1   3.0     0   0.0    56   5.0  declarationOf
     0   1.7     0   1.0     0   0.7     3   0.3  isSubtypeOf
     0   1.6     0   1.4     0   0.2     2   0.2  remoteLookup
     0   1.1     0   1.1     0   0.0     9   0.8  hasCycleOnSuperclassChain
     0   0.8     0   0.8     0   0.0    33   2.9  isUnknown
     0   0.7     0   0.7     0   0.0     1   0.1  isSuperTypeOf
     0   0.1     0   0.1     0   0.0     5   0.4  superClass
```

These profiles show the strategies and attributes that were evaluated
and how many times that happened.
E.g., the `obfuscateDecls` strategy was applied 75 times and the
`tipe` attribute 22 times.
The timing columns summarise the time taken as already described.

## Interactive use

If you supply the `-p` option without any dimensions, you will be
given a REPL into which you can enter dimensions interactively.
This approach means that you can examine different profile reports
without having to re-run the program.

E.g., running the PicoJava example with `-p` gives:

```
Profiler: enter a comma-separated list of dimension names, then enter (:q to exit)
> event
   183 ms total time
    57 ms profiled time (31.4%)
  1121 profile records

By event:

 Total Total  Self  Self  Desc  Desc Count Count
    ms     %    ms     %    ms     %           %
    33  58.4    33  58.1     0   0.2   874  78.0  StratEval
    24  41.9    24  41.9     0   0.0   247  22.0  AttrEval
> location
   183 ms total time
    57 ms profiled time (31.4%)
  1121 profile records

By location:

 Total Total  Self  Self  Desc  Desc Count Count
    ms     %    ms     %    ms     %           %
    36  64.1    11  20.0    25  44.1   155  13.8  Root
    35  60.9    19  33.8    15  27.2   264  23.6  Inner
    30  52.3    17  29.9    12  22.4   504  45.0  unknown location
    29  52.0     9  16.3    20  35.7   198  17.7  Leaf
>
```

## Kiama profiling dimensions

Strategy evaluations have the following dimensions:

  * `strategy`: a reference to the strategy that was evaluated,

  * `subject`: a reference to the node to which the strategy was applied, and

  * `result`: an option that is either `Some (t)` if the application was successful and resulted in the new term `t`, or `None` if the application failed.

Attribute evaluations have the following dimensions:

  * `attribute`: a reference to the attribute that was evaluated,

  * `subject`: a reference to the node on which the attribute was calculated,

  * `parameter`: an option that holds the parameter used with the attribute (if any),

  * `circular`: a Boolean that is true iff the attribute is defined to be a circular one,

  * `value`: the value that was computed for the attribute, and

  * `cached`: whether or not the attribute value was obtained from the attribute's cache.

The dimensions listed above are so-called _intrinsic dimensions_ since they
are a property of the primitive events that your program generates.
Kiama also supports the following _derived dimensions_ that are calculated
from the intrinsic dimensions.

  * `name`: gives the Scala-level name of the strategy or attribute that was used in an evaluation

  * `subjectHash`: for events with a `subject` dimension, gives the hash code of the subject. This dimension is useful if the `toString` of the subject doesn't allow you to distinguish between two subjects that are different references but print thee same.

  * `location`: for events with a `subject` dimension, gives the location of that subject node in a tree structure (one of "Root", "Inner" or "Leaf") if it can be determined. This dimension requires the nodes to be instances of Kiama's `Attributable` type.

  * `depends-on`: for attribute evaluations, summarises the immediate dependencies of the evaluation on other attributes and nodes

  * `dependencies`: outputs dot-format graphs for each attribute evaluation that show the transitive dependencies of that evaluation

## `ProfilingCompiler`

The support for the `-p` option shown above in the PicoJava example
is provided by the Kiama `ProfilingCompiler` trait.
You can use this trait in your own projects to obtain easy access
to the profiling functionality.
See the PicoJava example for details of how to use the trait.
The `ProfilingCompiler` trait illustrates how to interface with the
`dsprofile` library if you wish to do so directly.

Up: [User Manual](UserManual.md), Prev: [Read-eval-print Loops](ReadEvalPrintLoops.md)
