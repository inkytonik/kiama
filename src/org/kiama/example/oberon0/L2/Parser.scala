package org.kiama
package example.oberon0
package L2

/**
 * Parsers for L2 language.
 */
trait Parser extends L1.Parser {
    
    import base.source.Statement
    import L0.source.IdnExp
    import source.{Case, CaseStatement, ForStatement, MinMaxCond, ValCond}

    override def statementDef : PackratParser[Statement] =
        forStatement |
        caseStatement |
        super.statementDef
    
    lazy val forStatement =
        "FOR" ~> forVar ~ (":=" ~> expression) ~ ("TO" ~> expression) ~ step ~
             ("DO" ~> statementSequence <~ "END") ^^ ForStatement
 
    lazy val forVar =
        idnuse ^^ IdnExp
 
    lazy val step =
        "BY" ~> expression ^^ (e => Some (e)) |
        success (None)

    lazy val caseStatement =
        ("CASE" ~> expression <~ "OF") ~ cases ~ optelse <~ "END" ^^ CaseStatement

    lazy val cases =
        rep1sep (kase, "|") |
        failure ("clause expected")
        
    lazy val kase =
        conditions ~ (":" ~> statementSequence) ^^ Case
        
    lazy val conditions =
        rep1sep (condition, ",")

    lazy val condition =
        expression ~ (".." ~> expression) ^^ MinMaxCond |
        expression ^^ ValCond
             
    override def keywordStrings : List[String] =
        "BY" :: "CASE" :: "FOR" :: "OF" :: "STEP" :: "TO" :: super.keywordStrings

}
