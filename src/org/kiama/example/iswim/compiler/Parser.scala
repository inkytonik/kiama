/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Dominic R B Verity, Macquarie University.
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
package example.iswim.compiler

// TODO fix chronically bad parse error reporting.

/**
 * Parse ISWIM source code to an abstract syntax tree.
 */

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.input.Positional
import scala.util.matching.Regex
import scala.collection.immutable.HashSet

trait Parser extends JavaTokenParsers with PackratParsers {

    import Syntax._
    import scala.math._

    /**
     * Override handleWhiteSpace to ensure it handles comments appropriately.
     * Simply using a whiteSpace regex to recognise comments doesn't work with very
     * long comments - which appear to make Java's regex implementation fail.
     */

     // I've manged to tickle Java stack overflow and out of memory panics when trying
     // to match a 60 line comment with the Regex """(\s|/\*(?:.|[\n\r])*?\*/|//.*)+""".r

    override protected def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int = {
        def stripSpaces(offset : Int) : Int =
            ("""[^\s]""".r findFirstMatchIn source.subSequence(offset,source.length)) match {
                case None => source.length
                case Some(m) => stripComments(offset + m.start)
            }
        def stripComments(offset : Int) : Int = {
            val prefix = source.subSequence(offset,min(offset+2,source.length)).toString
            if (prefix == "/*")
                ("""\*/""".r findFirstMatchIn source.subSequence(offset+2,source.length)) match {
                    case None => source.length
                    case Some(m) => stripSpaces(offset + m.end + 2)
                }
            else if (prefix == "//")
                ("""[\n\r]""".r findFirstMatchIn source.subSequence(offset+2,source.length)) match {
                    case None => source.length
                    case Some(m) => stripSpaces(offset + m.end + 2)
                }
            else offset
        }
        stripSpaces(offset)
    }

    /**
     * Identifiers - keywords and names.
     */
    val reserved = HashSet(
          "if", "while", "let", "letrec", "in", "to"
        , "fun", "import", "callcc", "throw", "mkref"
        , "val", "true", "false", "primitives", "else"
        , "match", "and", "return"
        )

    lazy val identRegex : Regex = "[_a-zA-Z][_a-zA-Z0-9']*+".r
    lazy val intRegex : Regex = "[0-9]+".r

    case class Pos(s : String) extends Positional

    lazy val variable : Parser[Variable] = positioned (
            (identRegex | failure("variable name expected")) into { s =>
                if (reserved contains s)
                    failure("keyword \"" + s + "\" found where variable name expected")
                else
                    success(Variable(s))
                }
        )

    def keyword (k : String) : Parser[Pos] = positioned (
            (identRegex | failure("keyword expected")) into { s =>
                if (k == s)
                    success(Pos(s))
                else
                    failure("keyword \"" + k + "\" expected")}
        )

    def operator (op : String) : Parser[Pos] = (
            positioned (op ^^ Pos)
        |   failure("operator \"" + op + "\" expected")
        )


    /**
     * Literals, identifiers, keywords and other atomic phrases
     */
    lazy val liter : Parser[Expr] = (
            keyword("true") ^^ (p => BoolVal(true) setPos p.pos)
        |   keyword("false") ^^ (p => BoolVal(false) setPos p.pos)
        |   positioned(
                intRegex ^^ (s => NumVal(s.toInt))
            |   stringLiteral ^^ (s => StringVal(s.substring(1,s.length-1)))
            )
        )

    /**
     * Top level parsers
     */
    lazy val start : PackratParser[IswimProg] =  // removed importstmt
        phrase("" ~> positioned(rep1sep((letstmt | letrecstmt | primstmt | exprstmt),
                                  operator(";")) ^^ IswimProg))

    lazy val expr : PackratParser[Expr] =
        matchexpr | ifelseexpr | whileexpr | lambdaexpr | letexpr | letrecexpr | term4

    /**
    * Precedence based infix expression parsers.
    * Ordered from highest to lowest precedence.
    */
    lazy val parenexpr : PackratParser[Expr] =
        operator("(") ~> expr <~ operator(")")

    lazy val factor : PackratParser[Expr] =
        liter | variable | emptyexpr | tupleexpr | blockexpr | parenexpr

    lazy val applic : PackratParser[Expr] = (
            applic ~ factor ^^ {case a ~ f => Apply(a,f) setPos a.pos}
        |   operator("!") ~ applic ^^ {case p ~ f => Not(f) setPos p.pos}
        |   operator("-") ~ applic ^^ {case p ~ f => Negate(f) setPos p.pos}
        |   callccexpr | throwtoexpr | valexpr | mkrefexpr | returnexpr // removed primexpr
        |   factor
        )

    lazy val term1 : PackratParser[Expr] = (
            term1 ~ operator("*") ~ applic ^^ {case t ~ p ~ f => Times(t,f) setPos p.pos}
        |   term1 ~ operator("/") ~ applic ^^ {case t ~ p ~ f => Divide(t,f) setPos p.pos}
        |   term1 ~ operator("%") ~ applic ^^ {case t ~ p ~ f => Remainder(t,f) setPos p.pos}
        |   term1 ~ operator("&") ~ applic ^^ {case t ~ p ~ f => And(t,f) setPos p.pos}
        |   applic
        )

    lazy val term2 : PackratParser[Expr] = (
            term2 ~ operator("+") ~ term1 ^^ {case tl ~ p ~ tr => Plus(tl,tr) setPos p.pos}
        |   term2 ~ operator("-") ~ term1 ^^ {case tl ~ p ~ tr => Minus(tl,tr) setPos p.pos}
        |   term2 ~ operator("|") ~ term1 ^^ {case tl ~ p ~ tr => Or(tl,tr) setPos p.pos}
        |   term1
        )

    lazy val term3 : PackratParser[Expr] = (
            term2 ~ operator("==") ~ term2 ^^ {case tl ~ p ~ tr => Equal(tl,tr) setPos p.pos}
        |   term2 ~ operator("!=") ~ term2 ^^ {case tl ~ p ~ tr => NotEqual(tl,tr) setPos p.pos}
        |   term2 ~ operator("<") ~ term2 ^^ {case tl ~ p ~ tr => Less(tl,tr) setPos p.pos}
        |   term2 ~ operator("<=") ~ term2 ^^ {case tl ~ p ~ tr => LessEq(tl,tr) setPos p.pos}
        |   term2 ~ operator(">") ~ term2 ^^ {case tl ~ p ~ tr => Greater(tl,tr) setPos p.pos}
        |   term2 ~ operator(">=") ~ term2 ^^ {case tl ~ p ~ tr => GreaterEq(tl,tr) setPos p.pos}
        |   term2
        )

    lazy val term4 : PackratParser[Expr] = (
            term3 ~ operator(":=") ~ term4 ^^ {case tl ~ p ~ tr => Assign(tl,tr) setPos p.pos}
        |   term3
        )

    /**
    * Parse tuple / record expressions
    */
    lazy val emptyexpr : PackratParser[Expr] =
        operator("()") ^^ (p => Empty() setPos p.pos)

    lazy val tupleexpr : PackratParser[Expr] = (
            operator("(") ~ (expr <~ operator(",")) ~ rep1sep(expr, operator(","))
        <~  operator(")") ^^ {case p ~ e ~ es => Tuple(e::es) setPos p.pos})

    lazy val matchexpr : PackratParser[Expr] = (
            factor ~ keyword("match") ~ (operator("{")
        ~>  rep1sep(matchclause, operator(";")) <~ operator ("}"))
        ^^    {case ce ~ p ~ cs => Match(ce, cs) setPos p.pos}
        )

    lazy val pattern : PackratParser[(Pos, List[Variable])] = (
            operator("()") ^^ (p => (p,List()))
        |   variable ^^ (v => (Pos("") setPos v.pos, List(v)))
        |   operator("(") ~ rep1sep(variable, operator(",")) <~ operator(")") ^^
                {case p ~ vs => (p, vs)}
        )

    lazy val matchclause : PackratParser[MatchClause] =
        pattern ~ (operator("->") ~> expr) ^^
            { case (p, pat) ~ e =>
                MatchClause(Pattern(pat) setPos p.pos, e) setPos p.pos }

    /**
    * Parse code blocks
    */
    lazy val blockexpr : PackratParser[Expr] =
        operator("{") ~ (rep1sep(expr, operator(";")) <~ operator("}")) ^^
            {case p ~ ls => Block(ls) setPos p.pos}

    /**
    * Parse lambda (fun) expressions
    */
    lazy val lambdaexpr : PackratParser[Expr] =
        keyword("fun") ~ (operator("(") ~> variable <~ operator(")")) ~ expr ^^
            {case p ~ v ~ b => Lambda(v,b) setPos p.pos}

    /**
    * Parse return expressions
    */
    lazy val returnexpr : PackratParser[Expr] =
        keyword("return") ~ factor ^^ {case p ~ e => Return(e) setPos p.pos}

    /**
    * Parse let and letrec expressions
    */
    def bindexpr (rhsexpr : PackratParser[Expr]) : PackratParser[Binding] =
        variable ~ operator("=") ~ rhsexpr ^^ {case v ~ p ~ e => Binding(v,e) setPos p.pos}

    lazy val letexpr : PackratParser[Expr] =
        keyword("let") ~ rep1sep(bindexpr(expr),keyword("and")) ~ (keyword("in") ~> expr) ^^
            {case p ~ bs ~ e => Let(bs,e) setPos p.pos}

    lazy val letrecexpr : PackratParser[Expr] =
        keyword("letrec") ~ rep1sep(bindexpr(lambdaexpr),keyword("and")) ~
            (keyword("in") ~> expr) ^^ {case p ~ bs ~ e => LetRec(bs,e) setPos p.pos}

    /**
     * Parse toplevel statements
     */
    lazy val letstmt : PackratParser[Stmt] =
        keyword("let") ~ rep1sep(bindexpr(expr),keyword("and")) ^^
            {case p ~ bs => LetStmt(bs) setPos p.pos}

    lazy val letrecstmt : PackratParser[Stmt] =
        keyword("letrec") ~ rep1sep(bindexpr(lambdaexpr),keyword("and")) ^^
            {case p ~ bs => LetRecStmt(bs) setPos p.pos}

    lazy val exprstmt : PackratParser[Stmt] = blockexpr ^^ ExprStmt

    lazy val importstmt : PackratParser[Stmt] =
        keyword("import") ~ stringLiteral ^^
            {case p ~ s => Import(s.substring(1,s.length-1)) setPos p.pos}

    /**
    * Parse if ... then ... else ...
    */
    lazy val ifelseexpr : PackratParser[Expr] = (
            (keyword("if") ~ parenexpr ~ expr ~ (keyword("else") ~> expr) ^^
                {case p ~ e ~ thn ~ els => If(e,thn,els) setPos p.pos})
        )

    /**
    * Parse while expression
    */
    lazy val whileexpr : PackratParser[Expr] =
        keyword("while") ~ parenexpr ~ expr ^^
            {case p ~ e ~ b => While(e,b) setPos p.pos}

    /**
    * Continuation handling.
    */
    lazy val callccexpr : PackratParser[Expr] =
        keyword("callcc") ~ factor ^^ {case p ~ e => CallCC(e) setPos p.pos}

    lazy val throwtoexpr : PackratParser[Expr] =
        keyword("throw") ~ (expr <~ keyword("to")) ~ factor ^^
            {case p ~ v ~ c => ThrowTo(v,c) setPos p.pos}

    /**
    * References.
    */
    lazy val valexpr : PackratParser[Expr] =
        keyword("val") ~ factor ^^ {case p ~ e => Val(e) setPos p.pos}

    lazy val mkrefexpr : PackratParser[Expr] =
        keyword("mkref") ~ factor ^^ {case p ~ e => MkRef(e) setPos p.pos}

    /**
     * Primitives
     */
    lazy val primstmt : PackratParser[Stmt] =
        keyword("primitives") ~ rep1sep(variable,operator(",")) ^^
            {case p ~ vs => Primitives(vs) setPos p.pos}
}


