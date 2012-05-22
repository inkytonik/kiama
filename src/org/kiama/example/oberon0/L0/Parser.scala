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

    lazy val simpexp : PackratParser[Expression] =
        simpexp ~ ("+" ~> term) ^^ AddExp |
        simpexp ~ ("-" ~> term) ^^ SubExp |
        simpexp ~ ("OR" ~> term) ^^ OrExp |
        term

    lazy val term : PackratParser[Expression] =
        term ~ ("*" ~> factor) ^^ MulExp |
        term ~ ("DIV" ~> factor) ^^ DivExp |
        term ~ ("MOD" ~> factor) ^^ ModExp |
        term ~ ("&" ~> factor) ^^ AndExp |
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
