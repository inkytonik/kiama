package org.kiama
package example.oberon0
package L3.c

trait PrettyPrinter extends L1.c.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.c.{CASTNode, CType}
    import org.kiama.output.PrettyExpression

    override def toDoc (n : CASTNode) : Doc =
        n match {
            case CCall (s, ps) =>
                s <+> parens (hsep (ps map toParenDoc, comma)) <> semi

            case _ =>
                super.toDoc (n)
        }

    override def basetypeToDoc (t : CType) : Doc =
        t match {
            case CVoidType ()   => "void" <> space
            case CAddrType (bt) => super.basetypeToDoc (bt) <> "*"
            case _              => super.basetypeToDoc (t)
        }

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case CStrExp (s)  => dquotes (s)
            case _            => super.toParenDoc (e)
        }

}
