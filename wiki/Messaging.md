# Producing error messages using Kiama

Up: [User Manual](UserManual.md), Prev: [Pretty-printing](PrettyPrinting.md), Next: [Read-eval-print Loops](ReadEvalPrintLoops.md)

Kiama provides a simple messaging module to enable error messages to
be produced during an analysis, stored and then processed at a later
time, for example for output.

The following examples make particular use of the messaging module:

  * [Lambda2](Lambda2.md)

## Messaging

File: [org.bitbucket.inkytonik.kiama.util.Messaging.scala](http://code.google.com/p/kiama/source/browse/library/src/org/bitbucket/inkytonik/kiama/util/Messaging.scala)

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
[emitter](http://code.google.com/p/kiama/source/browse/core/src/org/bitbucket/inkytonik/kiama/util/Emitter.scala)
specifies how to actually do the output (default: print
to standard output).

Here are some examples of the message format. The position is printed
as `line.column`.

```
1.13: 'y' unknown
3.43: application of non-function
```

Up: [User Manual](UserManual.md), Prev: [Pretty-printing](PrettyPrinting.md), Next: [Read-eval-print Loops](ReadEvalPrintLoops.md)
