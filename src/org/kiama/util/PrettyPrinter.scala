/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2011 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.kiama.util

/**
 * The interface of a pretty printer using combinators from Swierstra and
 * Chitil (Linear, bounded, functional pretty-printing, Journal of Functional
 * Programming, 19 (1), 2008) and Leijen's PPrint library.  The latter
 * is a version of Wadler's library which was inspired by an earlier
 * library by Hughes.
 */
trait PrettyPrinterBase {

    /**
     * Indentation is expressed as integer space units.
     */
    type Indent = Int
    
    /**
     * Output medium width
     */
    type Width = Int

    /**
     * The final layout of a document
     */
    type Layout = String
    
    /**
     * Default indentation is four spaces.
     */
    val defaultIndent = 4
    
    /**
     * Default layout width is 75 characters.
     */
    val defaultWidth = 75
    
    /**
     * The operations provided by a pretty-printable document that don't
     * depend on the document's representation type.
     */
    trait DocOps {

        /**
         * Return the concatenation of this document with the argument.
         */
        def <> (e : Doc) : Doc
        
        // Extended operations, defined in terms of the basic operations.
            
        /**
         * Return the concatenation of this document with the argument
         * using a space separator.
         */
        def <+> (e : Doc) : Doc =
            this <> space <> e
        
        /**
         * Return the concatenation of this document with the argument
         * using a softline separator.
         */
        def </> (e : Doc) : Doc =
            this <> softline <> e
        
        /**
         * Return the concatenation of this document with the argument
         * using a softbreak separator.
         */
        def <\> (e : Doc) : Doc =
            this <> softbreak <> e
        
        /**
         * Return the concatenation of this document with the argument
         * using a line separator.
         */
        def <@> (e : Doc) : Doc =
            this <> line <> e
        
        /**
         * Return the concatenation of this document with the argument
         * using a linebreak separator.
         */
        def <@@> (e : Doc) : Doc =
            this <> linebreak <> e

    }
    
    /**
     * The representation type of pretty-printable documents.
     */
    type Doc <: DocOps
    
    // Output functions
    
    /**
     * Pretty print a document assuming a given output medium width.  In the paper
     * the width is the first parameter, but here we put it second so we can provide
     * a default value.
     */
    def pretty (d : Doc, w : Width = defaultWidth) : Layout
    
    /**
     * Pretty-print a pretty-printable value.  If the value passed is not a
     * pretty-printable document, it will be converted to one using the implicit
     * conversion anyToPrettyPrintable.
     */
    def pretty (p : PrettyPrintable) : Layout =
        pretty (p.toDoc)

    /**
     * Interface for pretty-printable values.  The default toDoc implementation
     * just returns a text document containing the toString of the receiver.
     */
    trait PrettyPrintable {
        def toDoc : Doc = text (toString)
    }

    /**
     * Convert any value into a pretty-printable value.  The value will
     * pretty-print as its toString result.
     */
    implicit def anyToPrettyPrintable (a : Any) : PrettyPrintable =
        new PrettyPrintable {
            override def toDoc = text (a.toString)
        }

    // Basic combinators.  Thse need to be implemented for a specific
    // instantiation of Doc.
    
    /**
     * Convert a string to a document.  The string should not contain any
     * newline characters.  Use line instead.
     */
    def text (t : String) : Doc
    
    /**
     * A document representing a potential line break.  Behaves like space
     * if the break is omitted by a group.
     */
    def line : Doc
    
    /**
     * A document representing a potential line break.  Behaves like empty
     * if the break is omitted by a group.
     */
    def linebreak : Doc
    
    /**
     * A document representing a choice among different ways to print a structure.
     */
    def group (d : Doc) : Doc
    
    /**
     * An empty document.  This is a left and right unit for the concatenation
     * method.  Called 'nil' in the paper.
     */
    def empty : Doc
    
    /**
     * Nest a document by an indentation increment on top of the current nesting.
     * In the paper version, the indentation parameter comes first, but we put it
     * second here so that it can be given a default value.
     */
    def nest (d : Doc, j : Indent = defaultIndent) : Doc
    
    // Extended comabinators that are implemented in terms of the basic 
    // combinators and the representation-independent document operations.

    /**
     * Convert a string to a document.  The string is allowed to contain
     * newline characters.  If no newlines are included, it is best to
     * use text directly instead.
     */
    def string (s : String) : Doc =
        if (s == "") {
            empty
        } else if (s (0) == '\n') {
            line <> string (s.tail)
        } else {
            val (xs, ys) = s.span (_ != '\n')
            text (xs) <> string (ys)
        }

    /**
     * Convert a character to a document.  The character can be a newline.
     */
    def char (c : Char) : Doc =
        if (c == '\n')
            line
        else
            text (c.toString)

    /**
     * Return a document that behaves like space if the resulting output
     * fits the page, otherwise it behaves like line.
     */
    def softline : Doc =
        group (line)

    /**
     * Return a document that behaves like empty if the resulting output
     * fits the page, otherwise it behaves like line.
     */
    def softbreak : Doc =
        group (linebreak)

    /**
     * Return a document representing n spaces if n > 0, otherwise return
     * an empty document.
     */
    def spaces (n : Int) : Doc =
        if (n <= 0)
            empty
        else
            text (" " * n)

    /**
     * Return a document that pretty-prints a list in Scala notation,
     * inserting line breaks between elements as necessary.
     * The prefix string can be changed from the default "List".
     * The elemToDoc argument can be used to alter the way each element
     * is converted to a document (default: call the element's toDoc
     * method).
     * sep defaults to a comma.
     */
    def list[T] (l : List[T], prefix : String = "List", 
                 elemToDoc : T => Doc = (x : T) => x.toDoc,
                 sep : Doc = comma) : Doc =
        text (prefix) <> parens (group (nest (lsep (l map elemToDoc, sep))))

    /**
    * Return a pretty-printer document for p.  If p is a Product, print it in
    * standard prefix list form, otherwise use p's toDoc method.  As a special
    * case, print lists as List (...) and Nil instead of using ::.  Also, 
    * strings are printed with surrounding double quotes.
    */
    def product (p : Any) : Doc = {
        p match {
            case Nil         => text ("Nil")
            case l : List[_] => list (l, "List ", product)
            case p : Product => list (p.productIterator.toList,
                                      p.productPrefix + " ",
                                      product)
            case s : String  => dquotes (text (s))
            case a           => a.toDoc
        }
    }

    // Extended combinator set

    /**
     * Return a document that concatenates the documents in the given sequence
     * either horizontally with <+> if they fit in the output medium width,
     * or if not, vertically with <@>.
     */
    def sep (ds : Seq[Doc]) : Doc =
        group (vsep (ds))

    /**
     * Helper fold.
     */
    private def fold (ds : Seq[Doc], f : (Doc, Doc) => Doc) =
        if (ds isEmpty)
            empty
        else
            ds.tail.foldLeft (ds.head) (f)

    /**
     * Return a document that concatenates the documents in the given sequence
     * horizontally with <+>.
     */
    def hsep (ds : Seq[Doc]) : Doc =
        fold (ds, (_ <+> _))

    /**
     * Return a document that concatenates the documents in the given sequence
     * vertically with <@>.
     */
    def vsep (ds : Seq[Doc]) : Doc =
        fold (ds, (_ <@> _))

    /**
     * Return a document that concatenates the documents in the given sequence
     * horizontally with <+> as long as they fit the output width, then
     * inserts a line and continues with the rest of the sequence.
     */
    def fillsep (ds : Seq[Doc]) : Doc =
        fold (ds, (_ </> _))

    /**
     * Return a document that concatenates the documents in the given sequence
     * and separates adjacent documents with sep with no space around the
     * separator.
     */
    def ssep (ds : Seq[Doc], sep : Doc) : Doc =
        fold (ds, (_ <> sep <> _))

    /**
     * Return a pretty-printer document for a separated sequence.
     * sep is the separator.  Line breaks are allowed before the sequence
     * and between the elements of the sequence.  The before one turns into
     * nothing if omitted.  The internal ones turn into spaces if omitted.
     */
    def lsep (ds : Seq[Doc], sep : Doc) : Doc =
        if (ds isEmpty)
            empty
        else
            linebreak <> fold (ds, _ <> sep <@> _)

    /**
     * Return a document that concatenates the documents in the given sequence
     * either horizontally with <> if they fit in the output medium width,
     * or if not, vertically with <@@>.
     */
    def cat (ds : Seq[Doc]) : Doc =
        group (vcat (ds))

    /**
     * Return a document that concatenates the documents in the given sequence
     * horizontally with <>.
     */
    def hcat (ds : Seq[Doc]) : Doc =
        fold (ds, (_ <> _))

    /**
     * Return a document that concatenates the documents in the given sequence
     * vertically with <@@>.
     */
    def vcat (ds : Seq[Doc]) : Doc =
        fold (ds, (_ <@@> _))

    /**
     * Return a document that concatenates the documents in the given sequence
     * horizontally with <> as long as they fit the output width, then
     * inserts a linebreak and continues to the end of the sequence.
     */
    def fillcat (ds : Seq[Doc]) : Doc =
        fold (ds, (_ <\> _))

    /**
     * Return a document that concatenates the documents in the given sequence
     * and terminates each document with term.
     */
    def sterm (ds : Seq[Doc], term : Doc) : Doc =
        cat (ds map (_ <> term))

    /**
     * Return a document representing a value formatted using toString.
     */
    def value (v : Any) : Doc =
        text (v.toString)

    /**
     * Return a document that encloses a given document d between two
     * occurrences of another document b.
     */
    def surround (d : Doc, b : Doc) : Doc =
        b <> d <> b

    /**
     * Return a document that encloses a given document between single
     * quotes.
     */
    def squotes (d : Doc) : Doc =
        surround (d, squote)

    /**
     * Return a document that encloses a given document between double
     * quotes.
     */
    def dquotes (d : Doc) : Doc =
        surround (d, dquote)

    /**
     * Return a document that encloses a given document between left
     * and right documents.
     */
    def enclose (l : Doc, d : Doc, r : Doc) : Doc =
        l <> d <> r

    /**
     * Return a document that encloses a given document between left
     * and right braces.
     */
    def braces (d : Doc) : Doc =
        enclose (lbrace, d, rbrace)

    /**
     * Return a document that encloses a given document between left
     * and right parentheses.
     */
    def parens (d : Doc) : Doc =
        enclose (lparen, d, rparen)

    /**
     * Return a document that encloses a given document between left
     * and right angle brackets.
     */
    def angles (d : Doc) : Doc =
        enclose (langle, d, rangle)

    /**
     * Return a document that encloses a given document between left
     * and right parentheses.
     */
    def brackets (d : Doc) : Doc =
        enclose (lbracket, d, rbracket)

    // Character shorthands

    /**
     * A left parenthesis document.
     */
    def lparen : Doc =
        char ('(')

    /**
     * A right parenthesis document.
     */
    def rparen : Doc =
        char (')')

    /**
     * A left angle bracket document.
     */
    def langle : Doc =
        char ('<')

    /**
     * A right angle bracket document.
     */
    def rangle : Doc =
        char ('>')

    /**
     * A left brace document.
     */
    def lbrace : Doc =
        char ('{')

    /**
     * A right brace document.
     */
    def rbrace : Doc =
        char ('}')

    /**
     * A left square bracket document.
     */
    def lbracket : Doc =
        char ('[')

    /**
     * A right square bracket document.
     */
    def rbracket : Doc =
        char (']')

    /**
     * A single quote document.
     */
    def squote : Doc =
        char ('\'')

    /**
     * A double quote document.
     */
    def dquote : Doc =
        char ('"')

    /**
     * A semicolon document.
     */
    def semi : Doc =
        char (';')

    /**
     * A colon document.
     */
    def colon : Doc =
        char (':')

    /**
     * A comma document.
     */
    def comma : Doc =
        char (',')

    /**
     * A space document.
     */
    def space : Doc =
        char (' ')

    /**
     * A dot (period) document.
     */
    def dot : Doc =
        char ('.')

    /**
     * A backslash document.
     */
    def backslash : Doc =
        char ('\\')

    /**
     * A forward slash document.
     */
    def forwslash : Doc =
        char ('/')

    /**
     * An equal sign document.
     */
    def equal : Doc =
        char ('=')

}

/**
 * A pretty-printer implemented using the continuation-based approach
 * from Section 3.3 of Swierstra, S., and Chitil, O. Linear, bounded,
 * functional pretty-printing. Journal of Functional Programming 19, 01
 * (2008), 1–16.
 *
 * defaultIndent specifies the indentation to use if none is specified in
 * uses of the nest method (defaults to 4). defaultWidth specifies the
 * default output medium width (defaults to 75).
 */
trait PrettyPrinter extends PrettyPrinterBase {

    import scala.collection.immutable.Queue
    import scala.collection.immutable.Queue.{empty => emptyDq}

    // Internal data types

    private type Remaining  = Int
    private type Horizontal = Boolean
    private type Out        = Remaining => Layout
    private type OutGroup   = Horizontal => Out => Out
    private type PPosition  = Int
    private type Dq         = Queue[(PPosition,OutGroup)]
    private type TreeCont   = (PPosition,Dq) => Out
    private type IW         = (Indent,Width)
    private type DocCont    = IW => TreeCont => TreeCont

    // Helper functions

    private def scan (l : Width, out : OutGroup) : TreeCont => TreeCont =
        (c : TreeCont) =>
            (p : PPosition, dq : Dq) =>
                if (dq.isEmpty) {
                    out (false) (c (p + l, emptyDq))
                } else {
                    val (s, grp) = dq.last
                    val n = (s, (h : Horizontal) => (grp (h)) compose (out (h)))
                    prune (c) (p + l, dq.init :+ n)
                }

    private def prune (c : TreeCont) : TreeCont =
        (p : PPosition, dq : Dq) =>
            (r : Remaining) =>
                if (dq.isEmpty) {
                    c (p, emptyDq) (r)
                } else {
                    val (s, grp) = dq.head
                    if (p > s + r) {
                        grp (false) (prune (c) (p, dq.tail)) (r)
                    } else {
                        c (p, dq) (r)
                    }
                }

    private def leave (c : TreeCont) : TreeCont =
        (p : PPosition, dq : Dq) =>
            if (dq.isEmpty) {
                c (p, emptyDq)
            } else if (dq.length == 1) {
                val (s1, grp1) = dq.last
                grp1 (true) (c (p, emptyDq))
            } else {
                val (s1, grp1) = dq.last
                val (s2, grp2) = dq.init.last
                val n = (s2, (h : Horizontal) =>
                                (c : Out) =>
                                    grp2 (h) ((r : Remaining) => grp1 (p <= s1 + r) (c) (r)))
                c (p, dq.init.init :+ n)
            }

    /**
     * Continuation representation of documents.
     */
    class Doc (f : DocCont) extends DocCont with DocOps {
        
        // Forward function operations to the function
        
        def apply (iw : IW) : TreeCont => TreeCont =
            f (iw)
        
        // Basic operations

        def <> (e : Doc) : Doc =
            new Doc (
                (iw : IW) =>
                    (this (iw)) compose (e (iw))
            )
    
    }

    // Basic combinators
    
    def text (t : String) : Doc =
        if (t == "")
            empty
        else
            new Doc (
                (iw : IW) => {
                    val l = t.length
                    val outText =
                        (_ : Horizontal) => (c : Out) => (r : Remaining) =>
                            t + c (r - l)
                    scan (l, outText)
                }
            )
    
    private def line (gap : Layout) : Doc =
        new Doc ({
            case (i, w) =>
                val outLine =
                    (h : Horizontal) => (c : Out) => (r : Remaining) =>
                        if (h) {
                            gap + c (r - gap.length)
                        } else {
                            '\n' + (" " * i) + c (w - i)
                        }
                scan (1, outLine)
        })
    
    def line : Doc =
        line (" ")

    def linebreak : Doc =
        line ("")
    
    def group (d : Doc) : Doc =
        new Doc (
            (iw : IW) =>
                (c : TreeCont) =>
                    (p : PPosition, dq : Dq) => {
                        val n = (h : Horizontal) => (c : Out) => c
                        d (iw) (leave (c)) (p, dq :+ (p, n))
                    }
        )
    
    def empty : Doc =
        new Doc (
            (iw : IW) =>
                (c : TreeCont) => c
        )
    
    def nest (d : Doc, j : Indent = defaultIndent) : Doc =
        new Doc ({
            case (i, w) =>
                d (i + j, w)
        })
        
    // Obtaining output

    def pretty (d : Doc, w : Width = defaultWidth) : Layout = {
        val c = (p : PPosition, dq : Dq) => (r : Remaining) => ""
        d (0, w) (c) (0, emptyDq) (w)
    }

}
