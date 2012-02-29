package org.kiama
package example.oberon0
package L4.source

trait PrettyPrinter extends L3.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.SourceASTNode
    import org.kiama.output.PrettyExpression

    override def toDoc (n : SourceASTNode) : Doc =
        n match {
            case ArrayTypeDef (s, t) =>
                "ARRAY" <+> toDoc (s) <+> "OF" <+> toDoc (t)

            case RecordTypeDef (Nil) =>
                "RECORD" <+> "END"

            case RecordTypeDef (fs) =>
                "RECORD" <+> hsep (fs map toDoc, semi) <+> "END"

            case FieldList (ids, t) =>
                (hsep (ids map text, comma)) <+> colon <+> toDoc (t)

            case _ =>
                super.toDoc (n)           
        }

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case IndexExp (b, e) =>
                toDoc (b) <> brackets (toDoc (e))
            case FieldExp (b, FieldIdn (f)) =>
                toDoc (b) <> "." <> f
            case _ =>
                super.toParenDoc (e)
        }

}
