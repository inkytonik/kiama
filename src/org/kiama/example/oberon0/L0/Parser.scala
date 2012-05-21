package org.kiama
package example.oberon0
package L0

/**
 * Parsers for L0 language.
 */
trait Parser extends base.Parser {

    import source.{AddExp, AndExp, Assignment, ConstDecl, DivExp, EqExp,
        Expression, GeExp, GtExp, IdnExp, IntExp, LeExp, LtExp, ModExp,
        MulExp, NamedType, NeExp, NegExp, NotExp, OrExp, SubExp, TypeDecl,
        TypeDef, VarDecl}

    override def declarationsDef =
        (constdeclsection?) ~ (typedeclsection?) ~ (vardeclsection?) ^^ {
            case oc ~ ot ~ ov =>
                List (oc, ot, ov).flatten.flatten
        }

    lazy val constdeclsection =
        "CONST" ~> rep (constdecl)

    lazy val constdecl =
        (idndef <~ "=") ~ (expression <~ ";") ^^ ConstDecl

    lazy val typedeclsection =
        "TYPE" ~> rep (typedecl)

    lazy val typedecl =
        (idndef <~ "=") ~ (typedef <~ ";") ^^ TypeDecl

    lazy val vardeclsection =
        "VAR" ~> rep (vardecl)

    lazy val vardecl =
        (idndeflist <~ ":") ~ (typedef <~ ";") ^^ VarDecl

    lazy val idndeflist =
        rep1sep (idndef, ",")

    lazy val typedef =
        typedefDef

    def typedefDef : PackratParser[TypeDef] =
        namedtypedef

    lazy val namedtypedef =
        idnuse ^^ NamedType

    override def statementDef =
        assignment |
        super.statementDef

    lazy val assignment =
        lhs ~ (":=" ~> expression) ^^ Assignment

    lazy val lhs =
        lhsNoPos

    lazy val lhsNoPos =
        lhsDef

    def lhsDef : PackratParser[Expression] =
        idnuse ^^ IdnExp

    lazy val expression =
        simpexp ~ ("=" ~> simpexp) ^^ EqExp |
        simpexp ~ ("#" ~> simpexp) ^^ NeExp |
        simpexp ~ ("<" ~> simpexp) ^^ LtExp |
        simpexp ~ ("<=" ~> simpexp) ^^ LeExp |
        simpexp ~ (">" ~> simpexp) ^^ GtExp |
        simpexp ~ (">=" ~> simpexp) ^^ GeExp |
        simpexp

    lazy val simpexp =
        nsimpexp

    lazy val nsimpexp : PackratParser[Expression] =
        nsimpexp ~ ("+" ~> term) ^^ AddExp |
        nsimpexp ~ ("-" ~> term) ^^ SubExp |
        nsimpexp ~ ("OR" ~> term) ^^ OrExp |
        term

    lazy val term =
        nterm

    lazy val nterm : PackratParser[Expression] =
        nterm ~ ("*" ~> factor) ^^ MulExp |
        nterm ~ ("DIV" ~> factor) ^^ DivExp |
        nterm ~ ("MOD" ~> factor) ^^ ModExp |
        nterm ~ ("&" ~> factor) ^^ AndExp |
        factor

    lazy val factor : PackratParser[Expression] =
        intexp |
        lhs |
        "+" ~> factor |
        "-" ~> factor ^^ NegExp |
        "~" ~> factor ^^ NotExp |
        "(" ~> expression <~ ")"

    lazy val intexp =
        constrainedInt ^^ IntExp

    override def keywords =
        "CONST" :: "DIV" :: "MOD" :: "OR" :: "TYPE" :: "VAR" :: super.keywords

}
