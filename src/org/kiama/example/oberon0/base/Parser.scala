package org.kiama
package example.oberon0
package base

import org.kiama.util.WhitespacePositionedParserUtilities

/**
 * Parsers for base language.
 */
trait Parser extends WhitespacePositionedParserUtilities {
  
    import source.{Block, Declaration, EmptyStmt, IdnDef, IdnUse,
        ModuleDecl, Statement}

    lazy val parser =
        phrase (moduledecl)

    lazy val moduledecl =
        "MODULE" ~> (idndef <~ ";") ~ block ~ (idnuse <~ ".") ^^ ModuleDecl

    // Statement sequences produce blocks, which is not strictly necessary
    // for the source language but it makes transformation easier since
    // declarations can be added to the blocks later. At the moment there
    // will never be any declarations from parsing, but we leave this action
    // general in case that changes.

    lazy val block =
        declarations ~ statements ^^ {
            case ds1 ~ Block (ds2, ss) =>
                Block (ds1 ++ ds2, ss)
        }

    lazy val declarations =
        declarationsDef

    def declarationsDef : PackratParser[List[Declaration]]
        "" ^^^ Nil

    lazy val statements =
        "BEGIN" ~> statementSequence <~ "END" |
        "END" ^^^ Block (Nil, Nil)

    lazy val statementSequence =
        rep1sep (statement, ";") ^^ {
            case ss =>
                Block (Nil, ss)
        }

    lazy val statement =
        statementDef
        
    def statementDef : PackratParser[Statement] =
        result (EmptyStmt ())

    lazy val idndef =
        ident ^^ IdnDef

    lazy val idnuse =
        ident ^^ IdnUse

    def keywords =
        List ("BEGIN", "END", "MODULE")

    lazy val keyword =
        regex (keywords.mkString ("|").r)
        
    lazy val ident =
        not (keyword) ~> "[a-zA-Z][a-zA-Z0-9]*".r |
        failure ("ident expected")

    lazy val whitespaceParser : PackratParser[Any] = 
        rep (whiteSpace | comment)
        
    lazy val comment : PackratParser[Any] =
        "(*" ~ rep (not ("*)") ~ (comment | any)) ~ "*)"

}
