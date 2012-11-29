package org.kiama
package example.oberon0
package L1.source

trait PrettyPrinter extends L0.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.{Block, SourceASTNode}
    import L0.source.Expression

    override def toDoc (n : SourceASTNode) : Doc =
        n match {
            case s : IfStatement =>
                ifToDoc (s)

            case s : WhileStatement =>
                "WHILE" <+> toDoc (s.cond) <+> "DO" <> semisep (s.block.stmts) <@> "END"

            case _ =>
                super.toDoc (n)
        }

    def ifToDoc (s : IfStatement) : Doc = {

        def elsifToDoc (ei : (Expression, Block)) : Doc =
            line <> "ELSIF" <+> toDoc (ei._1) <+> "THEN" <> semisep (ei._2.stmts)

        "IF" <+> toDoc (s.cond) <+> "THEN" <>
        semisep (s.block.stmts) <>
        hcat (s.elsifs map elsifToDoc) <>
        s.optelse.map (b => line <> "ELSE" <> semisep (b.stmts)).getOrElse (empty) <@>
        "END"
    }

}
