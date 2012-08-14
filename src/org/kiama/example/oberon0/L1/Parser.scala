package org.kiama
package example.oberon0
package L1

/**
 * Parsers for L1 language.
 */
trait Parser extends L0.Parser {
    
    import base.source.Block
    import L0.source.Expression
    import source.{IfStatement, WhileStatement}

    override def statementDef =
        ifStatement |
        whileStatement |
        super.statementDef
    
    lazy val ifStatement =
        "IF" ~> expression ~ ("THEN" ~> statementSequence) ~
            elsifs ~ (optelse <~ "END") ^^ IfStatement
        
    lazy val elsifs =
        rep (elsif)
    
    lazy val elsif : PackratParser[(Expression, Block)] =
        ("ELSIF" ~> expression) ~ ("THEN" ~> statementSequence)
        
    lazy val optelse =
        "ELSE" ~> statementSequence ^^ (ss => Some (ss)) |
        success (None)

    lazy val whileStatement =
        "WHILE" ~> expression ~ ("DO" ~> statementSequence <~ "END") ^^
        WhileStatement
        
    override def keywordStrings =
        "DO" :: "ELSE" :: "ELSIF" :: "IF" :: "THEN" :: "WHILE" :: super.keywordStrings

}
