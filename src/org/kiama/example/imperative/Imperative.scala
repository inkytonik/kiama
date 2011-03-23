/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2011 Anthony M Sloane, Macquarie University.
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

package org.kiama
package example.imperative

import org.kiama.util.GeneratingREPL
import org.kiama.util.ParsingREPL
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.RegexParsers

/**
 * A simple imperative language abstract syntax designed for testing.
 */
object AST {
    
    import org.kiama.rewriting.Rewriter.{Strategy, rulefs, congruence}
    import scala.util.parsing.input.Positional

    /**
     * Identifiers are represented as strings.
     */
    type Idn = String

    /**
     * Superclass of all imperative language tree node types.
     */
    trait ImperativeNode extends Product with Positional

    /**
     * Expressions.
     */
    abstract class Exp extends ImperativeNode {

        /**
         * The numeric value of the expression.
         */
        def value : Double

        /**
         * The set of all variable references in the expression.
         */
        def vars : Set[Idn] = Set ()

        /**
         * The number of divisions by the constant zero in the expression.
         */
        def divsbyzero : Int = 0

        /**
         * The depth of the expression, i.e., the number of levels from the
         * root to the leaf values.
         */
        def depth : Int = 0

        /**
         * The number of additions of integer constants in the expression.
         */
        def intadds : Int = 0
    }

    /**
     * Numeric expressions.
     */
    case class Num (d : Double) extends Exp {
        override def value = d
        override def depth = 2
    }

    /**
     * Variable expressions.
     */
    case class Var (s : Idn) extends Exp {
        // Hack to make tests more interesting
        override def value = 3
        override def vars = Set (s)
        override def depth = 2
        override def toString = "Var(\"" + s + "\")"
    }

    /**
     * Unary negation expressions.
     */
    case class Neg (e : Exp) extends Exp {
        override def value = - e.value
        override def vars = e.vars
        override def divsbyzero = e.divsbyzero
        override def depth = 1 + e.depth
        override def intadds = e.intadds
    }

    /**
     * Binary expressions.
     */
    abstract class Binary (l : Exp, r : Exp) extends Exp {
        override def vars = l.vars ++ r.vars
        override def divsbyzero = l.divsbyzero + r.divsbyzero
        override def depth = 1 + (l.depth).max (r.depth)
        override def intadds = l.intadds + r.intadds
    }

    /**
     * Addition expressions.
     */
    case class Add (l : Exp, r : Exp) extends Binary (l, r) {
        override def value = l.value + r.value
        override def intadds =
            (l, r) match {
                case (Num (_), Num (_)) => 1
                case _                  => super.intadds
            }
    }

    /**
     * Subtraction expressions.
     */
    case class Sub (l : Exp, r : Exp) extends Binary (l, r) {
        override def value = l.value - r.value
    }

    /**
     * Multiplication expressions.
     */
    case class Mul (l : Exp, r : Exp) extends Binary (l, r) {
        override def value = l.value * r.value
    }

    /**
     * Division expressions.
     */
    case class Div (l : Exp, r : Exp) extends Binary (l, r) {
        // Hack: no errors, so return zero for divide by zero
        override def value = if (r.value == 0) 0 else l.value / r.value
        override def divsbyzero =
            l.divsbyzero + (r match {
                                case Num (0) => 1
                                case _       => r.divsbyzero
                            })
    }

    /**
     * Statements.
     */
    abstract class Stmt extends ImperativeNode {

        /**
         * The set of all variable references in the statement.
         */
        def vars : Set[Idn] = Set ()

    }

    /**
     * Empty statements.
     */
    case class Null () extends Stmt

    /**
     * Statement sequences.
     */
    case class Seqn (ss : Seq[Stmt]) extends Stmt {
        override def vars = Set (ss flatMap (_ vars) : _*)
    }

    /**
     * Assignment statements.
     */
    case class Asgn (v : Var, e : Exp) extends Stmt {
        override def vars = Set (v.s)
    }

    /**
     * While loops.
     */
    case class While (e : Exp, b : Stmt) extends Stmt {
        override def vars = e.vars ++ b.vars
    }

    // Congruences

    def Num (s1 : => Strategy) : Strategy =
        rulefs {
            case _ : Num =>
                congruence (s1)
        }

    def Var (s1 : => Strategy) : Strategy =
        rulefs {
            case _ : Var =>
                congruence (s1)
        }

    def Neg (s1 : => Strategy) : Strategy =
        rulefs {
            case _ : Var =>
                congruence (s1)
        }

    def Add (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : Add =>
                congruence (s1, s2)
        }

    def Sub (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : Sub =>
                congruence (s1, s2)
        }

    def Mul (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : Mul =>
                congruence (s1, s2)
        }

    def Div (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : Div =>
                congruence (s1, s2)
        }

    def Seqn (s1 : => Strategy) : Strategy =
        rulefs {
            case _ : Seqn =>
                congruence (s1)
        }

    def Asgn (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : Asgn =>
                congruence (s1, s2)
        }

    def While (s1 : => Strategy, s2 : => Strategy) : Strategy =
        rulefs {
            case _ : While =>
                congruence (s1, s2)
        }

}

/**
 * A version of the imperative expression abstract syntax built without
 * using case classes.  Used for testing on non-case class data structures.
 */
object ASTNonCase {
    
    import org.kiama.rewriting.Rewritable
    import org.kiama.rewriting.Rewriter.Term

    type Idn = String

    abstract class Exp extends Rewritable

    class Num (val d : Double) extends Exp {
        def arity = 1
        def deconstruct = List (d)
        def reconstruct (cs : Array[Term]) = 
            cs match {
                case Array (d : Double) =>
                    new Num (d)
                case _ =>
                    illegalArgs ("Num", "Double", cs)
            }
        override def toString = "Num(" + d + ")"
    }

    class Var (val s : Idn) extends Exp {
        def arity = 1
        def deconstruct = List (s)
        def reconstruct (cs : Array[Term]) = 
            cs match {
                case Array (s : Idn) =>
                    new Var (s)
                case _ =>
                    illegalArgs ("Var", "Idn", cs)
            }
        override def toString = "Var(" + s + ")"
    }

    class Neg (val e : Exp) extends Exp {
        def arity = 1
        def deconstruct = List (e)
        def reconstruct (cs : Array[Term]) = 
            cs match {
                case Array (e : Exp) =>
                    new Neg (e)
                case _ =>
                    illegalArgs ("Neg", "Exp", cs)
            }
        override def toString = "Neg(" + e + ")"
    }

    abstract class Binary (val l : Exp, val r : Exp) extends Exp {
        def arity = 2
        def deconstruct = List (l, r)
    }

    class Add (l : Exp, r : Exp) extends Binary (l, r) {
        def reconstruct (cs : Array[Term]) = 
            cs match {
                case Array (l : Exp, r : Exp) =>
                    new Add (l, r)
                case _ =>
                    illegalArgs ("Add", "Exp, Exp", cs)
            }
        override def toString = "Add(" + l + "," + r + ")"
    }
    
    class Sub (l : Exp, r : Exp) extends Binary (l, r) {
        def reconstruct (cs : Array[Term]) = 
            cs match {
                case Array (l : Exp, r : Exp) =>
                    new Sub (l, r)
                case _ =>
                    illegalArgs ("Sub", "Exp, Exp", cs)
            }
        override def toString = "Sub(" + l + "," + r + ")"
    }
    
    class Mul (l : Exp, r : Exp) extends Binary (l, r) {
        def reconstruct (cs : Array[Term]) = 
            cs match {
                case Array (l : Exp, r : Exp) =>
                    new Mul (l, r)
                case _ =>
                    illegalArgs ("Mul", "Exp, Exp", cs)
            }
        override def toString = "Mul(" + l + "," + r + ")"
    }
    
    class Div (l : Exp, r : Exp) extends Binary (l, r) {
        def reconstruct (cs : Array[Term]) = 
            cs match {
                case Array (l : Exp, r : Exp) =>
                    new Div (l, r)
                case _ =>
                    illegalArgs ("Div", "Exp, Exp", cs)
            }
        override def toString = "Div(" + l + "," + r + ")"
    }

}

/**
 * AST pretty-printing.
 */
object PrettyPrinter extends org.kiama.util.PrettyPrinter {

    import AST._

    /**
     * Return a pretty-printed version of a node.
     */
    def pretty (t : ImperativeNode) : String =
        super.pretty (show (t))

    /**
     * Convert an imperative node to a pretty-printing document in
     * fully-parenthesised C style.
     */
    def show (t : ImperativeNode) : Doc =
        t match {
            case Num (d)      => value (d)
            case Var (s)      => text (s)
            case Neg (e)      => parens (text ("-") <> show (e))
            case Add (l, r)   => showbin (l, "+", r)
            case Sub (l, r)   => showbin (l, "-", r)
            case Mul (l, r)   => showbin (l, "*", r)
            case Div (l, r)   => showbin (l, "/", r)
            case Null ()      => semi
            case Seqn (ss)    => braces (nest (line <> ssep (ss map show, line)) <> line)
            case Asgn (v, e)  => show (v) <+> text ("=") <+> show (e) <> semi
            case While (e, b) => text ("while") <+> parens (show (e)) <> nest (line <> show (b))
        }

    /**
     * Return a pretty-printing document for an instance of a binary expression.
     */
    def showbin (l : ImperativeNode, op : String, r : ImperativeNode) : Doc =
        parens (show (l) <+> text (op) <+> show (r))

}

/**
 * Parser to AST.
 */
trait Parser extends RegexParsers with PackratParsers {

    import AST._

    lazy val start : PackratParser[Stmt] =
        phrase (stmt)

    lazy val stmt : PackratParser[Stmt] =
        ";" ^^^ Null () | sequence | asgnStmt | whileStmt

    lazy val asgnStmt : PackratParser[Asgn] =
        variable ~ ("=" ~> exp) <~ ";" ^^ { case v ~ e => Asgn (v, e) }

    lazy val whileStmt : PackratParser[While] =
        ("while" ~> "(" ~> exp <~ ")") ~ stmt ^^ { case e ~ b => While (e, b) }

    lazy val sequence : PackratParser[Seqn] =
        "{" ~> (stmt*) <~ "}" ^^ Seqn

    lazy val exp : PackratParser[Exp] =
        exp ~ ("+" ~> term) ^^ { case l ~ r => Add (l, r) } |
        exp ~ ("-" ~> term) ^^ { case l ~ r => Sub (l, r) } |
        term

    lazy val term : PackratParser[Exp] =
        term ~ ("*" ~> factor) ^^ { case l ~ r => Mul (l, r) } |
        term ~ ("/" ~> factor) ^^ { case l ~ r => Div (l, r) } |
        factor

    lazy val factor : PackratParser[Exp] =
        double | integer | variable | "-" ~> exp ^^ Neg | "(" ~> exp <~ ")"

    lazy val double : PackratParser[Num] =
        """[0-9]+\.[0-9]+""" ^^ (s => Num (s.toDouble))

    lazy val integer : PackratParser[Num] =
        "[0-9]+".r ^^ (s => Num (s.toInt))

    lazy val variable : PackratParser[Var] =
        idn ^^ Var

    lazy val idn : PackratParser[String] =
        not (keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r

    lazy val keyword : Parser[String] =
        "while"

}

/**
 * ScalaCheck generators for programs in the imperative language.
 */
trait Generator {

    import org.scalacheck._
    import AST._

    val genInteger = for (i <- Gen.choose (1, 100)) yield Num (i)
    val genDouble = for (i <- Gen.choose (1.0, 1000000.0)) yield Num (i)
    val genNum = Gen.frequency ((3, genInteger), (1, genDouble))

    implicit def arbNum : Arbitrary[Num] =
        Arbitrary (genNum)

    val genIdn : Gen[String] = for (s <- Gen.identifier) yield (s.take (5))
    val genVar = for (v <- genIdn) yield Var (v)

    val genLeafExp = Gen.oneOf (genNum, genVar)

    def genNeg (sz : Int) =
        for { e <- genExp (sz/2) } yield Neg (e)

    def genAdd (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Add (l, r)

    def genSub (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Sub (l, r)

    def genMul (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Mul (l, r)

    def genDiv (sz : Int) =
        for { l <- genExp (sz/2); r <- genExp (sz/2) } yield Div (l, r)

    def genInternalExp (sz : Int) =
        Gen.oneOf (genAdd (sz), genSub (sz), genMul (sz), genDiv (sz))

    def genExp (sz : Int) : Gen[Exp] =
        if (sz <= 0)
            genLeafExp
        else
            Gen.frequency ((1, genLeafExp), (3, genInternalExp (sz)))

    implicit def arbExp : Arbitrary[Exp] =
        Arbitrary { Gen.sized (sz => genExp (sz)) }

    val genLeafStmt = Gen.value (Null ())

    def genSeqn (sz : Int) =
        for { len <- Gen.choose (1,sz)
              ss <- Gen.containerOfN[List,Stmt] (len, genStmt (sz / len)) }
            yield Seqn (ss)

    implicit def arbSeqn : Arbitrary[Seqn] =
        Arbitrary { Gen.sized (sz => genSeqn (sz)) }

    def genAsgn (sz : Int) =
        for { v <- genVar; e <- genExp (sz-1) } yield Asgn (v, e)

    implicit def arbAsgn : Arbitrary[Asgn] =
        Arbitrary { Gen.sized (sz => genAsgn (sz)) }

    def genWhile (sz : Int) =
        for { e <- genExp (sz/3); b <- genStmt (sz - 1) } yield While (e, b)

    implicit def arbWhile : Arbitrary[While] =
        Arbitrary { Gen.sized (sz => genWhile (sz)) }

    def genInternalStmt (sz : Int) : Gen[Stmt] =
        Gen.frequency ((1, genSeqn (sz)), (5, genAsgn (sz)), (3, genWhile (sz)))

    def genStmt (sz : Int) =
        if (sz <= 0)
            genLeafStmt
        else
            Gen.frequency ((1, genLeafStmt), (9, genInternalStmt (sz)))

    implicit def arbStmt : Arbitrary[Stmt] =
        Arbitrary { Gen.sized (sz => genStmt (sz)) }

}

/**
 * Basis for tests using the imperative language.  Includes support for generating
 * random AST instances plus convenient access to the parser and pretty-printer.
 */
trait TestBase extends Generator with Parser

/**
 * A read-eval-print loop for parsing imperative programs and printing thei
 * abstract synax trees.
 */
object Imperative extends ParsingREPL[AST.Stmt] with Parser {

    override def setup (args : Array[String]) : Boolean = {
        println ("Enter imperative language programs for parsing.")
        true
    }

    override def prompt () = "imperative> "

    def process (s : AST.Stmt) {
        println (s)
        println (PrettyPrinter.pretty (s))
    }

}

/**
 * A read-eval-print loop for generating random imperative statements.
 */
object ImperativeGen extends GeneratingREPL[AST.Stmt] with Generator {

    def generator () = arbStmt

    override def process (s : AST.Stmt) {
        println (s)
        println (PrettyPrinter.pretty (s))
    }

}
