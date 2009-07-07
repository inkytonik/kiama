Kiama

Kiama is a Scala library for language processing.  In the Kiama project we are
investigating embedding of language processing formalisms such as grammars,
parsers, rewriters and analysers into general-purpose programming languages.

This document provides a short overview of Kiama and how to build it.  See
"Licensing" for information on distributing Kiama and including it in your
programs.

IMPORTANT NOTE: Kiama is a research project, it's early days and the code is
undergoing heavy development, so many details will change.  Consult with us
before you rely on it for serious work.  We make no guarantees about the
features or performance of the Kiama library if you do choose to use it.

Anthony Sloane

Programming Languages Research Group
Department of Computing, Macquarie University

Anthony.Sloane@mq.edu.au
inkytonik@gmail.com

http://plrg.science.mq.edu.au/
http://www.comp.mq.edu.au/~asloane

1. Obtaining Kiama

Kiama is hosted on Google Code at

    http://kiama.googlecode.com

The sources can be checked out of our Mercurial repository using the
following command:

    hg clone http://kiama.googlecode.com/hg/ kiama

2. Contents

The source distribution of Kiama is structured as follows:

src/
    attribution         Dynamically-schedule attribute grammar library
    example             Example language specifications that use Kiama
    parsing             Packrat parsing library                            
    rewriting           Strategy-based rewriting library
lib/                    Third-party libraries
        
Tests for each section of the library are located in the relevant
source directories.        
        
3. Requirements

To build and use Kiama you need the following software:

* Scala (http://www.scala-lang.org/)
  Tested with version 2.7.5
  
* Java (http://www.java.com)
  Tested with version 1.5 on Mac OS X 10.5.6
  
The following libraries are also needed.  Compatible versions are
distributed with the Kiama source distribution in the lib directory.

* JLine (http://jline.sourceforge.net/)
  Tested with version 0.9.94
  
* JUnit (http://www.junit.org)
  Tested with version 4.5

* ScalaCheck (http://code.google.com/p/scalacheck/)
  Tested with version 1.5
  
* ScalaTest (http://www.artima.com/scalatest/)
  Tested with version 0.9.5

4. Building

Kiama is most easily built using ant.  If you have the SCALA_HOME environment
variable set to the location of your Scala distribution, then you should not
need to modify build.xml.  This distribution has been tested with
Scala 2.7.5.

Kiama can also be built using Eclipse.  The source distribution is an
Eclipse project.  Builds have most recently been tested with Eclipse 3.4
and version 2.7.5 of the Eclipse Scala plugin.

Alternatively, you may want to examine and modify the build.xml settings
for scala.home, scala.lib, scalacheck.lib, scalatest.lib and junit.lib as
appropriate.

Build kiama with "ant build" or just "ant" since "build" is the default.  The
build should compile the library into the "bin" directory.  To run Kiama-based
code you will need to put this directory on your CLASSPATH.

Other useful ant targets are:

clean      remove all generated files
doc        generate the API documentation into bin/doc/api
test       run all of the tests using the ScalaTest GUI
testbatch  run all of the tests in batch mode
dist       make a distribution jar of the Kiama library and sources in dist directory

5. Mailing lists

There are two Google Groups for Kiama:

kiama           General announcements and discussions
                http://groups.google.com/group/kiama
                kiama@googlegroups.com
            
kiama-commit    Commit messages and Hudson build problems
                http://groups.google.com/group/kiama-commit
                kiama-commit@googlegroups.com

6. Acknowledgements

Contributors to the Kiama code have been:

Tony Sloane
Lennart Kats (particularly in attribution)
Ben Mockler (the Oberon-0 example)

Kiama is currently concentrating on incorporating existing language processing
formalisms, so credit goes to the original developers of those formalisms.  See
the code for details of the sources of ideas that come from elsewhere.

Most of the build process has been adapted from that used by Rickard Nilsson
in ScalaCheck.

Some parts of this library are based on code in the Scala Library,
most notably the parsing library.  See http://www.scala-lang.org/node/216.

Many of the library rewriting strategies are based on the Stratego library.
See http://releases.strategoxt.org/docs/api/libstratego-lib/stable/docs/.

7. Licensing

Kiama is distributed under the GNU Lesser General Public License.  See the files
COPYING and COPYING.LESSER for details of these licenses.  More information can
be found at http://www.gnu.org/licenses/.

Some code in this library has been adapted from the Scala library.  This is
indicated by a note in the file header and an EPFL copyright notice.  See the
file COPYING.SCALA for the details of the Scala license.

Kiama includes the following third-party libraries:

    JLine (http://jline.sourceforge.net/)

Licenses that apply to the third party libraries follow.

* JLine

Copyright (c) 2002-2006, Marc Prud'hommeaux <mwp1@cornell.edu>
All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the following
conditions are met:

Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with
the distribution.

Neither the name of JLine nor the names of its contributors
may be used to endorse or promote products derived from this
software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
OF THE POSSIBILITY OF SUCH DAMAGE.

