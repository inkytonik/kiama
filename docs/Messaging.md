# Producing error messages using Kiama

Up: [User Manual](UserManual), Prev: [Pretty-printing](PrettyPrinting), Next: [Read-eval-print Loops](ReadEvalPrintLoops)

IMPORTANT NOTE: This page describes Kiama 1.x. Much of it also applies
to Kiama 2.x, but not all. Please consult the 2.x release notes for the
main differences. We are currently writing comprehensive documentation
for 2.x that will eventually replace these pages.

Kiama provides a simple messaging module to enable error messages to
be produced during an analysis, stored and then processed at a later
time, for example for output.

The following examples make particular use of the messaging module:

  * [Lambda2](Lambda2)

## Messaging

File: [org.bitbucket.inkytonik.kiama.util.Messaging.scala](http://code.google.com/p/kiama/source/browse/library/src/main/scala/org/bitbucket/inkytonik/kiama/util/Messaging.scala)

The `Messaging` object implements the messaging module. To store the
messages it contains a list buffer, each item of which contains position
information and a message string.

```
case class Record (pos : Position, message : String)
val messages : ListBuffer[Record]
```

`Messaging` provides a simple interface to update and query the
message sequence. `message` records a message associated with a
particular positioned value.  The value must implement
`scala.util.parsing.input.Positional` so that the position information
can be obtained.

```
def message (value : Positional, message : String)
```

`messagecount` returns a count of the number of messages currently
stored.

```
def messagecount : Int
```

`resetmessages` clears the message sequence.

```
def resetmessages ()
```

## Processing messages

Client code can traverse the message sequence directly if desired.
Also, `report` can be used to sort the list and print each message
in a standard format.

```
def report (emitter : Emitter = new Emitter)
```

The
[emitter](http://code.google.com/p/kiama/source/browse/core/src/main/scala/org/bitbucket/inkytonik/kiama/util/Emitter.scala)
specifies how to actually do the output (default: print
to standard output).

Here are some examples of the message format. The position is printed
as `line.column`.

```
1.13: 'y' unknown
3.43: application of non-function
```

Up: [User Manual](UserManual), Prev: [Pretty-printing](PrettyPrinting), Next: [Read-eval-print Loops](ReadEvalPrintLoops)
