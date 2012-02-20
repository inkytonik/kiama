package org.kiama
package example.oberon0
package L0.c

import org.kiama.util.ParenPrettyPrinter

trait PrettyPrinter extends base.c.PrettyPrinter {

    this : org.kiama.util.PrettyPrinter =>

    import base.c.{CASTNode, CExpression, CType}
    import org.kiama.util.PrettyExpression

    override def toDoc (n : CASTNode) : Doc =
        n match {
            case CNamedType (s) =>
                s

            case CTypeDef (d) =>
                "typedef" <+> toDoc (d)

            case CInitDecl (d, e) =>
                toDoc (d) <+> "=" <+> toDoc (e)

            case CAssignment (d, e) =>
                toDoc (d) <+> "=" <+> toDoc (e) <> semi

            case e : CExpression =>
                toParenDoc (e)

            case _ =>
                super.toDoc (n)
        }

    override def basetypeToDoc (t : CType) : Doc =
        t match {
            case CNamedType (i) => i <> space
            case _              => super.basetypeToDoc (t)
        }

    /**
     * CNegExp (CNegExp) special case is to avoid output of --e which is interpreted
     * as a pre-decrement operator.
     */
    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case CIdnExp (i)           => i
            case CNegExp (e : CNegExp) => "-" <> parens (toParenDoc (e))
            case _                     => super.toParenDoc (e)
        }

}
