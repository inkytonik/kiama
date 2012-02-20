package org.kiama
package example.oberon0
package L4.c

trait PrettyPrinter extends L3.c.PrettyPrinter {
    
    this : org.kiama.util.PrettyPrinter =>

    import base.c.{CASTNode, CType}
    import L3.c.CDerefExp
    import org.kiama.util.PrettyExpression

    override def basetypeToDoc (t : CType) : Doc =
        t match {
            case CRecordType (fls) =>
                "struct" <+> "{" <> (nest (lterm (fls map toDoc, semi))) <>
                    line <> "}" <> space
            case _ =>
                super.basetypeToDoc (t)
        }

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case CIndexExp (a, e) =>
                toDoc (a) <> brackets (toDoc (e))
            case CFieldExp (r : CDerefExp, f) =>
                parens (toDoc (r)) <> dot <> f
            case CFieldExp (r, f) =>
                toDoc (r) <> dot <> f
            case _ =>
                super.toParenDoc (e)
        }

}
