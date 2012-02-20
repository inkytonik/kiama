package org.kiama
package example.oberon0
package L3.source

trait PrettyPrinter extends L2.source.PrettyPrinter {

    this : org.kiama.util.PrettyPrinter =>

    import base.source.{IdnDef, IdnUse, SourceASTNode}

    override def toDoc (n : SourceASTNode) : Doc =
        n match {
            case ProcDecl (IdnDef (i1), as, b, IdnUse (i2)) =>
                "PROCEDURE" <+> i1 <> paramsToDoc (as map toDoc, semi) <> semi <@>
                blockToDoc (b, true) <+> i2 <> semi

            case FPSection (m, ids, t) =>
                val mode : Doc = if (m == VarMode) "VAR " else empty
                mode <> idlistToDoc (ids) <+> colon <+> toDoc (t)

            case Call (IdnUse (i), ps) =>
                i <> paramsToDoc (ps map toParenDoc, comma)
                
            case _ =>
                super.toDoc (n)           
        }

    def paramsToDoc (ds : List[Doc], sep : Doc) : Doc =
        ds match {
            case Nil => empty
            case _   => space <> parens (hsep (ds, sep))
        }

}
