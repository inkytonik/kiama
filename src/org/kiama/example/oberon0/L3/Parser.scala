package org.kiama
package example.oberon0
package L3

/**
 * Parsers for L3 language.
 */
trait Parser extends L2.Parser {

    import base.source.{Declaration, Statement}
    import source.{Call, FPSection, ProcDecl, ValMode, VarMode}

    override def declarationsDef : PackratParser[List[Declaration]] =
        super.declarationsDef ~ rep (procedureDeclaration <~ ";") ^^ {
            case ds ~ pds => ds ++ pds
        }

    lazy val procedureDeclaration =
        ("PROCEDURE" ~> idndef) ~ (optformalParameters <~ ";") ~ block ~ idnuse ^^ ProcDecl

    lazy val optformalParameters : PackratParser[List[FPSection]] =
        "(" ~> repsep (fpsection, ";") <~ ")" |
        result (Nil)

    lazy val fpsection =
        optvar ~ (idndeflist <~ ":") ~ typedef ^^ FPSection

    lazy val optvar =
        "VAR" ^^^ VarMode () |
        result (ValMode ())

    override def statementDef : PackratParser[Statement] =
        procedureCall |
        super.statementDef

    lazy val procedureCall =
        idnuse ~ optActualParameters ^^ Call

    lazy val optActualParameters =
        "(" ~> repsep (expression, ",") <~ ")" |
        guard (";" | "ELSE" | "END") ^^^ Nil

    override def keywordStrings : List[String] =
        "PROCEDURE" :: super.keywordStrings

}
