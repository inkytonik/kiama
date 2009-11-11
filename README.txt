Kiama

Kiama is a Scala library for language processing.  In the Kiama project we are
investigating embedding of language processing formalisms such as grammars,
parsers, rewriters and analysers into general-purpose programming languages.

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

1. The Kiama project site at Google code

Information about how to build, install and use Kiama can be found on
the Kiama project site:

kiama.googlecode.com

2. Documentation and mailing lists

The main documentation for Kiama takes the form of wiki pages
covering library features and examples, available at the Google
Code site.  The User Manual page is a good place to start:

    http://code.google.com/p/kiama/wiki/UserManual

For summary information about Kiama releases, including dependencies
on other software and links to API documentation, see the Releases
wiki page:

    http://code.google.com/p/kiama/wiki/Releases
    
Installation instructions can be found here:

    http://code.google.com/p/kiama/wiki/Installation

There are also two Google Groups for Kiama:

kiama           General announcements and discussions
                http://groups.google.com/group/kiama
                kiama@googlegroups.com
            
kiama-commit    Commit messages and Hudson build problems
                http://groups.google.com/group/kiama-commit
                kiama-commit@googlegroups.com

3. Acknowledgements

Contributors to the Kiama code have been:

Tony Sloane
Lennart Kats (particularly in attribution)
Ben Mockler (the Oberon-0 example)

Kiama is currently concentrating on incorporating existing language processing
formalisms, so credit goes to the original developers of those formalisms.  See
the code for details of the sources of ideas that come from elsewhere.

Some parts of this library are based on code in the Scala Library,
most notably the parsing library.  See http://www.scala-lang.org/node/216.

Many of the library rewriting strategies are based on the Stratego library.
See http://releases.strategoxt.org/docs/api/libstratego-lib/stable/docs/.

4. Licensing

Kiama is distributed under the GNU Lesser General Public License.  See the files
COPYING and COPYING.LESSER for details of these licenses.  More information can
be found at http://www.gnu.org/licenses/.

Licenses that apply to code from which Kiama code was derived are
provided in COPYING.OTHER.
