package org.kiama
package example.oberon0
package L2.source

trait PrettyPrinter extends L1.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.SourceASTNode

    override def toDoc (n : SourceASTNode) : Doc =
        n match {
            case s : ForStatement =>
                forToDoc (s)

            case s : CaseStatement =>
                caseToDoc (s)
                
            case _ =>
                super.toDoc (n)           
        }

    def forToDoc (s : ForStatement) : Doc =
        "FOR" <+> toDoc (s.idn) <+> ":=" <+> toDoc (s.lower) <+>
            "TO" <+> toDoc (s.upper) <+>
            s.by.map (e => "BY" <+> toDoc (e) <> space).getOrElse (empty) <>
            "DO" <> semisep (s.block.stmts) <@> "END"

    def caseToDoc (s : CaseStatement) : Doc =
        "CASE" <+> toDoc (s.exp) <+> "OF" <>
        casesToDoc (s.cases) <@>
        s.optelse.map (b => "ELSE" <> semisep (b.stmts) <> line).getOrElse (empty) <>
        "END"

    def casesToDoc (l : List[Case]) : Doc = {

        def condToDoc (cond : Condition) : Doc =
            cond match {
                case ValCond (e) =>
                    toDoc (e)
                case MinMaxCond (min, max) =>
                    toDoc (min) <+> ".." <+> toDoc (max)
            }

        def condsToDoc (conds : List[Condition]) : Doc =
            hsep (conds map condToDoc, comma)

        def singleCaseToDoc (kase : Case) : Doc =
            condsToDoc (kase.conds) <+> ":" <+>
            hsep (kase.block.stmts map toDoc, semi)

        nest (line <> "  " <> ssep (l map singleCaseToDoc, line <> "| "), 2)
        
    }
    
}
