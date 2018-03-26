# Kiama installation from binary or source

## Installing from a Maven repository

IMPORTANT NOTE: This page describes Kiama 1.x. Much of it also applies
to Kiama 2.x, but not all. Please consult the 2.x release notes for the
main differences. We are currently writing comprehensive documentation
for 2.x that will eventually replace these pages.

Kiama is published in the Central Maven repository.
Tools such as the [Scala simple-build-tool (sbt)](http://www.scala-sbt.org)
and Maven will be able to install Kiama automatically from this repository.
The build systems will automatically download the libraries on which Kiama depends.
The easiest way to use Kiama is via one of these build systems.

If you are new to sbt, we strongly advise you to read the [sbt Getting Started Guide](http://www.scala-sbt.org/release/docs/Getting-Started/Welcome.html).

If you are using Kiama in an sbt project, you should add a dependence on

```
"org.bitbucket.inkytonik.kiama" %% "kiama" % "version"
```

where "version" is the version number of the Kiama library that you
want to use. The `%%` indicates that the correct version of Kiama will
be used for your Scala build version. See the
[search facility on Maven Central](http://search.maven.org/#search%7Cga%7C1%7Ckiama)
for the available versions.

For example, you can add a library dependence on version 1.8.0 of Kiama
by including the following in your `build.sbt`:

```
libraryDependencies ++=
    Seq (
        "org.bitbucket.inkytonik.kiama" %% "kiama" % "1.8.0",
        ... other libraries here ...
    )
```

The Kiama test library contains a collection of examples, tests that use
those examples, and useful test support code. If you want to use the Kiama test
library in your project tests, add a dependency of the following form to your
sbt build configuration.

```
"org.bitbucket.inkytonik.kiama" %% "kiama" % "version" % "test" classifier ("test")
```

Kiama before version 2.0.0 was published under the "com.googlecode.kiama" (1.2.0-1.8.0) and "com.googlecode" (before 1.2.0) organisations, so you will need to use
that ID if you want to use an older version of the library.

A [giter8](http://github.com/n8han/giter8#readme) template for a sample
Kiama-based sbt project is [available](https://github.com/inkytonik/kiama.g8).
The template contains simple examples of basic Kiama features including parsing,
pretty-printing, rewriting, attribution, read-eval-print-loops and testing.
It's a good way to easily take a look at what is possible.

## Downloading a binary version

Jars for Kiama can can be obtained from
[Maven Central](http://search.maven.org/#search%7Cga%7C1%7Ckiama).
Nightly builds are available on [CloudBees](https://inkytonik.ci.cloudbees.com/job/Kiama).

## Running using a binary version

If you are not using a build tool such as sbt,
to use Kiama with your own code you will need to include the Kiama jar
on the class path of your `scalac` and `scala` invocations or in the
class path of your IDE project. See the Scala
[getting started documentation](http://www.scala-lang.org/node/166) or
the [documentation for the Scala IDE plugins](http://www.scala-lang.org/node/91#ide_plugins)
for more information.

To use some optional parts of Kiama such as the tests, you will also need
to add some third-party libraries to your class path.
The [release page](http://code.google.com/p/kiama/wiki/Releases) details the
relevant libraries and versions.

## Using the "bleeding edge" version

If you want to use the most recent changes to Kiama you can try the
[latest nightly SNAPSHOT build](https://inkytonik.ci.cloudbees.com/job/Kiama/).
At this location you will find the latest version that built successfully.
We make no guarantee that a Kiama library obtained in this
way will actually work in all cases, but it will have passed all of
the tests.

## Obtaining Kiama's source

If you prefer to build Kiama from source, you first need to install
the [Mercurial](http://www.selenic.com/mercurial/) version control
system. Then, download the latest version of Kiama from our Mercurial
repository using the following command.

```
hg clone https://kiama.googlecode.com/hg/ kiama
```

If you want to download a specific released version, use a command
like this instead

```
hg clone -r v1.0.2 https://kiama.googlecode.com/hg/ kiama
```

where `v1.0.2` is the tag of the release.

Tests for each section of the library are located in the relevant
source directories.

## Building Kiama from source

Kiama is built using the [Scala simple-build-tool (sbt)](http://www.scala-sbt.org).
You will need to have a
working Java installation on your machine to run sbt. Also, a working
network connection is needed for the first build so that the correct
versions of Scala and the libraries on which Kiama depends can be
downloaded before the build. Subsequent builds do not require a
network connection unless you delete the downloaded files.

Builds and related operations are achieved using the sbt script that
you set up as part of your sbt installation. E.g., the following
commands show how to build the library on a Unix-like system.

To download the library dependencies run the `sbt` command as follows.
This command only needs to be run once unless you delete the libraries.

```
sbt update
```

To compile the Kiama library, run

```
sbt compile
```

This will place the class files in the `target` directory.

You can generate a report from running the Kiama tests using

```
sbt test
```

No news is good news.  You may find that your JVM settings mean
that the tests will not complete, failing with a stack overflow
or heap space exhaustion.
The solution is to modify your `sbt` script to add options
such as `-Xss2M` to increase the available stack space and
`-Xmx1G` to increase the available heap space.

You can create API documentation for the library using

```
sbt doc
```

The documentation will be located in the `target/scala-$scalaversion/api` directory
where `$scalaversion` is the version of the Scala compiler.

Finally, a jar file can be created for use elsewhere using the command

```
sbt package
```

which will create `target/scala-$scalaversion/kiama_$scalaversion-$kiamaversion.jar` where
`$kiamaversion` is the current Kiama version number.

sbt also has an interactive mode that is engaged by just running

```
sbt
```

sbt commands can then be run from the sbt prompt without some of the
start-up overhead.

The compile, test, doc and package commands are the most commonly used
sbt commands. For full details of sbt usage please see the
[Scala simple-build-tool (sbt) documentation](http://www.scala-sbt.org).
