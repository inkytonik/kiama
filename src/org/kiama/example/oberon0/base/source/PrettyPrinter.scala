package org.kiama
package example.oberon0
package base.source

trait PrettyPrinter extends SourcePrettyPrinter {

    this : org.kiama.util.PrettyPrinter =>

    def declsection (d : Declaration) : String =
        ""

    def toDoc (n : SourceASTNode) : Doc =
        n match {
            case ModuleDecl (IdnDef (i1), Block (Nil, Nil), IdnUse (i2)) =>
                "MODULE" <+> i1 <> semi <@> "END" <+> i2 <> dot

            case ModuleDecl (IdnDef (i1), b, IdnUse (i2)) =>
                "MODULE" <+> i1 <> semi <@> blockToDoc (b, true) <+> i2 <> dot

            case b : Block =>
                blockToDoc (b)

            case _ =>
                empty
        }

    /**
     * Pretty-print a block, omitting the BEGIN if there are no statements.
     * No declarations can be present at this level.  Second parameter says
     * whether the BEGIN-END should be included if there are no declarations.
     */
    def blockToDoc (b : Block, beginend : Boolean = false) : Doc =
        b.stmts match {
            case Nil => "END"
            case ss  =>
                if (beginend)
                    "BEGIN" <> semisep (ss) <@> "END"
                else
                    vsep (ss map toDoc, semi)
        }

    /**
     * Pretty-print a nested list of nodes separated by sep (default: semi
     * colon) and line breaks.
     */
    def semisep (l : List[SourceASTNode], sep : Doc = semi) : Doc =
        nest (lsep (l map toDoc, sep))

}
