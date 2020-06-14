# Kiama release documentation

See the [user manual](UserManual.md) for a tutorial-style description of
Kiama features and usage and the [installation](Installation.md) manual
for details of installation of binary releases or building from
source.

This page summarises all numbered Kiama releases. Links to the API
documentation and information about versions of compatible tools and
libraries are also given. A version of Scala is needed and each release
has been tested with the version shown. The optional libraries are not
needed for basic usage of Kiama, but will be needed for more
advanced usage as noted.  In each case, the listed compatible version
is known to work, but other versions may also.

The easiest way to use Kiama with your own Scala code is to build your
project with [Scala simple-build-tool (sbt)](http://www.scala-sbt.org) (or a
similar tool such as Maven).  See the [Installation](Installation.md) page for details.
Since Kiama is built with sbt, a source build of Kiama will also automatically
download the compatible tools and libraries so there is no need to download
them manually when building from source.

  * Version 2.3.0 for Scala 2.10, 2.11, 2.12 and 2.13 (December 18, 2019)
    * [Release notes](doc/2.3.0/notes.html)
    * [2.3.0 API documentation](http://www.javadoc.io/doc/org.bitbucket.inkytonik.kiama/kiama_2.13/2.3.0)
    * Compatible tools and libraries:
      * [Scala 2.10.x/2.11.x/2.12.x/2.13.x](http://www.scala-lang.org)
      * [dsinfo 0.3.0/0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0/0.4.0](https://github.com/inkytonik/dsprofile)
      * [Guava collections 21.0](https://github.com/google/guava)
      * To use the read-eval-print loops:
        * [JLine 2.14.6](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 3.3.2](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.14.3](https://github.com/rickynils/scalacheck)
        * [ScalaTest 3.1.0](http://www.artima.com/scalatest/)

  * Version 2.2.1 for Scala 2.10, 2.11 and 2.12 (April 26, 2019)
    * [Release notes](doc/2.2.1/notes.html)
    * [2.2.1 API documentation](http://www.javadoc.io/doc/org.bitbucket.inkytonik.kiama/kiama_2.12/2.2.1)
    * Compatible tools and libraries:
      * [Scala 2.10.x/2.11.x/2.12.x](http://www.scala-lang.org)
      * [dsinfo 0.3.0/0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0/0.4.0](https://github.com/inkytonik/dsprofile)
      * [Guava collections 27.1-jre](https://github.com/google/guava)
      * To use the read-eval-print loops:
        * [JLine 2.14.6](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 3.2.0](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.14.0](https://github.com/rickynils/scalacheck)
        * [ScalaTest 3.0.7](http://www.artima.com/scalatest/)

  * Version 2.2.0 for Scala 2.10, 2.11 and 2.12 (April 11, 2018)
    * [Release notes](doc/2.2.0/notes.html)
    * [2.2.0 API documentation](http://www.javadoc.io/doc/org.bitbucket.inkytonik.kiama/kiama_2.12/2.2.0)
    * Compatible tools and libraries:
      * [Scala 2.10.x/2.11.x/2.12.x](http://www.scala-lang.org)
      * [dsinfo 0.3.0/0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0/0.4.0](https://github.com/inkytonik/dsprofile)
      * [Guava collections 24](https://github.com/google/guava)
      * To use the read-eval-print loops:
        * [JLine 2.14.6](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 3.1.2](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.13.5](https://github.com/rickynils/scalacheck)
        * [ScalaTest 3.0.5](http://www.artima.com/scalatest/)

  * Version 2.1.0 for Scala 2.10, 2.11 and 2.12 (June 19, 2017)
    * [Release notes](doc/2.1.0/notes.html)
    * [2.1.0 API documentation](http://www.javadoc.io/doc/org.bitbucket.inkytonik.kiama/kiama_2.12/2.1.0)
    * Compatible tools and libraries:
      * [Scala 2.10.x/2.11.x/2.12.x](http://www.scala-lang.org)
      * [dsinfo 0.3.0/0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0/0.4.0](https://github.com/inkytonik/dsprofile)
      * [Guava collections 21](https://github.com/google/guava)
      * To use the read-eval-print loops:
        * [JLine 2.14.3](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 2.1.1](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.13.5](https://github.com/rickynils/scalacheck)
        * [ScalaTest 3.0.3](http://www.artima.com/scalatest/)

  * Version 2.0.0 for Scala 2.10 and 2.11 (April 6, 2016)
    * [Release notes](doc/2.0.0/notes.html)
    * [2.0.0 API documentation](http://www.javadoc.io/doc/org.bitbucket.inkytonik.kiama/kiama_2.11/2.0.0)
    * Compatible tools and libraries:
      * [Scala 2.10.x/2.11.x](http://www.scala-lang.org)
      * [dsinfo 0.3.0/0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0/0.4.0](https://github.com/inkytonik/dsprofile)
      * [Guava collections 19](https://github.com/google/guava)
      * To use the read-eval-print loops:
        * [JLine 2.14.1](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 1.0.0](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.12.5](https://github.com/rickynils/scalacheck)
        * [ScalaTest 2.2.5](http://www.artima.com/scalatest/)

  * Version 1.8.0 for Scala 2.10 and 2.11 (November 10, 2014)
    * [Release notes](doc/1.8.0/notes.html)
    * [1.8.0 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.11/1.8.0)
    * Compatible tools and libraries:
      * [Scala 2.10.x/2.11.x](http://www.scala-lang.org)
      * [dsinfo 0.3.0/0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0/0.4.0](https://github.com/inkytonik/dsprofile)
      * [Guava collections 17](https://github.com/google/guava)
      * To use the read-eval-print loops:
        * [JLine 2.12](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 0.9.5](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.11.4](https://github.com/rickynils/scalacheck)
        * [ScalaTest 2.2.0](http://www.artima.com/scalatest/)

  * Version 1.7.0 for Scala 2.10 and 2.11 (August 11, 2014)
    * [Release notes](doc/1.7.0/notes.html)
    * [1.7.0 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.11/1.7.0)
    * Compatible tools and libraries:
      * [Scala 2.10.x/2.11.x](http://www.scala-lang.org)
      * [dsinfo 0.3.0/0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0/0.4.0](https://github.com/inkytonik/dsprofile)
      * [Guava collections 17](https://github.com/google/guava)
      * To use the read-eval-print loops:
        * [JLine 2.12](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 0.9.5](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.11.4](https://github.com/rickynils/scalacheck)
        * [ScalaTest 2.2.0](http://www.artima.com/scalatest/)

  * Version 1.6.0 for Scala 2.10 and 2.11 (May 16, 2014)
    * [Release notes](doc/1.6.0/notes.html)
    * [1.6.0 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.11/1.6.0)
    * Compatible tools and libraries:
      * [Scala 2.10.x/2.11.x](http://www.scala-lang.org)
      * [dsinfo 0.3.0/0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0/0.4.0](https://github.com/inkytonik/dsprofile)
      * [Guava collections 17](https://github.com/google/guava)
      * To use the read-eval-print loops:
        * [JLine 2.11](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 0.9.5](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.11.4](https://github.com/rickynils/scalacheck)
        * [ScalaTest 2.1.3](http://www.artima.com/scalatest/)

  * Version 1.5.3 for Scala 2.11 (April 21, 2014)
    * [Release notes](doc/1.5.3/notes.html)
    * [1.5.3 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.11/1.5.3)
    * Compatible tools and libraries:
      * [Scala 2.11.x](http://www.scala-lang.org)
      * [dsinfo 0.4.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.4.0](https://github.com/inkytonik/dsprofile)
      * To use the read-eval-print loops:
        * [JLine 2.11](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 0.9.5](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.11.3](https://github.com/rickynils/scalacheck)
        * [ScalaTest 2.1.3](http://www.artima.com/scalatest/)

  * [Version 1.5.2](http://kiama.googlecode.com/files/kiama_2.10-1.5.2.jar) (December 22, 2013)
    * [Release notes](doc/1.5.2/notes.html)
    * [1.5.2 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.10/1.5.2)
    * Compatible tools and libraries:
      * [Scala 2.10.x](http://www.scala-lang.org)
      * [dsinfo 0.2.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.3.0](https://github.com/inkytonik/dsprofile)
      * To use the read-eval-print loops:
        * [JLine 2.11](https://github.com/jline/jline2)
      * Command-line support in examples:
        * [Scallop 0.9.4](https://github.com/scallop/scallop)
      * To run Kiama tests:
        * [ScalaCheck 1.10.1](https://github.com/rickynils/scalacheck)
        * [ScalaTest 2.0](http://www.artima.com/scalatest/)

  * [Version 1.5.1](http://kiama.googlecode.com/files/kiama_2.10-1.5.1.jar) (July 5, 2013)
    * [Release notes](doc/1.5.1/notes.html)
    * [1.5.1 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.10/1.5.1)
    * Compatible tools and libraries:
      * [Scala 2.10.x](http://www.scala-lang.org)
      * [dsinfo 0.2.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.2.0](https://github.com/inkytonik/dsprofile)
      * To use the read-eval-print loops:
        * [JLine 1.0](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [ScalaCheck 1.10.0](https://github.com/rickynils/scalacheck)
        * [ScalaTest 1.9.1](http://www.artima.com/scalatest/)

  * [Version 1.5.0](http://kiama.googlecode.com/files/kiama_2.10-1.5.0.jar) (May 14, 2013)
    * [Release notes](doc/1.5.0/notes.html)
    * [1.5.0 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.10/1.5.0)
    * Compatible tools and libraries:
      * [Scala 2.10.x](http://www.scala-lang.org)
      * [dsinfo 0.2.0](https://github.com/inkytonik/dsinfo)
      * [dsprofile 0.2.0](https://github.com/inkytonik/dsprofile)
      * To use the read-eval-print loops:
        * [JLine 1.0](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [ScalaCheck 1.10.0](https://github.com/rickynils/scalacheck)
        * [ScalaTest 1.9.1](http://www.artima.com/scalatest/)

  * [Version 1.4.0 for Scala 2.9.2](http://kiama.googlecode.com/files/kiama_2.9.2-1.4.0.jar) [Version 1.4.0 for Scala 2.10](http://kiama.googlecode.com/files/kiama_2.10-1.4.0.jar) (January 2, 2013)
    * [Release notes](doc/1.4.0/notes.html)
    * [1.4.0 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.10/1.4.0)
    * Compatible tools and libraries:
      * [Scala 2.9.2, 2.10.0](http://www.scala-lang.org)
      * To use the read-eval-print loops:
        * [JLine 1.0](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [ScalaCheck 1.10.0](https://github.com/rickynils/scalacheck)
        * [ScalaTest 1.9.1](http://www.artima.com/scalatest/)

  * [Version 1.3.0](http://kiama.googlecode.com/files/kiama_2.9.2-1.3.0.jar) (July 12, 2012)
    * [Release notes](doc/1.3.0/notes.html)
    * [1.3.0 API documentation](http://www.javadoc.io/doc/com.googlecode.kiama/kiama_2.9.2/1.3.0)
    * Compatible tools and libraries:
      * [Scala 2.9.2](http://www.scala-lang.org)
      * To use the read-eval-print loops:
        * [JLine 2.7](https://github.com/jline/jline2)
      * To run Kiama tests:
        * [JUnit 4.10](http://www.junit.org)
        * [ScalaCheck 1.9](https://github.com/rickynils/scalacheck)
        * [ScalaTest 1.8](http://www.artima.com/scalatest/)
        * [Argot 0.4](http://software.clapper.org/argot/)

  * [Version 1.2.0](http://kiama.googlecode.com/files/kiama_2.9.1-1.2.0.jar) (March 8, 2012)
    * [Release notes](doc/1.2.0/notes.html)
    * Compatible tools and libraries:
      * [Scala 2.9.1](http://www.scala-lang.org)
      * To use the read-eval-print loops:
        * [JLine 1.0.0](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [JUnit 4.10](http://www.junit.org)
        * [ScalaCheck 1.9](http://code.google.com/p/scalacheck/)
        * [ScalaTest 1.7.1](http://www.artima.com/scalatest/)
        * [Argot 0.3.5](http://software.clapper.org/argot/)

  * [Version 1.1.0](http://kiama.googlecode.com/files/kiama_2.9.0-1.1.0.jar) (May 18, 2011)
    * [Release notes](doc/1.1.0/notes.html)
    * Compatible tools and libraries:
      * [Scala 2.9.0](http://www.scala-lang.org)
      * To run Kiama tests:
        * [JUnit 4.8.1](http://www.junit.org)
        * [ScalaCheck 1.9](http://code.google.com/p/scalacheck/)
        * [ScalaTest 1.4](http://www.artima.com/scalatest/)

  * [Version 1.0.2](http://kiama.googlecode.com/files/kiama_2.8.0-1.0.2.jar) (January 4, 2011)
    * [Release notes](doc/1.0.2/notes.html)
      * [Scala 2.8.1](http://www.scala-lang.org)
      * To use the read-eval-print loops:
        * [JLine 0.9.4](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [JUnit 4.8.1](http://www.junit.org)
        * [ScalaCheck 1.8](http://code.google.com/p/scalacheck/)
        * [ScalaTest 1.2](http://www.artima.com/scalatest/)

  * [Version 1.0.1](http://kiama.googlecode.com/files/kiama_2.8.0-1.0.1.jar) (September 20, 2010)
    * [Release notes](doc/1.0.1/notes.html)
    * Compatible tools and libraries:
      * [Scala 2.8.0](http://www.scala-lang.org)
      * To use the read-eval-print loops:
        * [JLine 0.9.4](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [JUnit 4.8.1](http://www.junit.org)
        * [ScalaCheck 1.7](http://code.google.com/p/scalacheck/)
        * [ScalaTest 1.2](http://www.artima.com/scalatest/)

  * [Version 1.0.0](http://kiama.googlecode.com/files/kiama_2.8.0-1.0.0.jar) (July 17, 2010)
    * [Release notes](doc/1.0.0/notes.html)
    * Compatible tools and libraries:
      * [Scala 2.8.0](http://www.scala-lang.org)
      * To use the read-eval-print loops:
        * [JLine 0.9.4](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [JUnit 4.8.1](http://www.junit.org)
        * [ScalaCheck 1.7](http://code.google.com/p/scalacheck/)
        * [ScalaTest 1.2](http://www.artima.com/scalatest/)

  * [Version 0.9](http://kiama.googlecode.com/files/kiama-0.9.0.jar) (November 12, 2009)
    * Changes since previous release:
      * use sbt instead of ant for building
      * attribution is less dependent on Attributable
      * bug fixes and small improvements
    * Compatible tools and libraries:
      * [Scala 2.7.7](http://www.scala-lang.org)
      * To use the read-eval-print loops:
        * [JLine 0.9.4](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [JUnit 4.7](http://www.junit.org)
        * [ScalaCheck 1.5](http://code.google.com/p/scalacheck/)
        * [ScalaTest 1.0](http://www.artima.com/scalatest/)

  * [Version 0.8](http://kiama.googlecode.com/files/kiama-0.8.jar) (August 4, 2009)
    * Compatible tools and libraries:
      * [Scala 2.7.5](http://www.scala-lang.org)
      * To use the read-eval-print loops:
        * [JLine 0.9.4](http://jline.sourceforge.net/)
      * To run Kiama tests:
        * [JUnit 4.5](http://www.junit.org)
        * [ScalaCheck 1.5](http://code.google.com/p/scalacheck/)
        * [ScalaTest 0.9.5](http://www.artima.com/scalatest/)
