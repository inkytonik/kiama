package org.kiama
package example.oberon0
package L1.c

trait PrettyPrinter extends L0.c.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.c.CASTNode

    override def toDoc (n : CASTNode) : Doc =
        n match {
            case CIfStatement (c, ts) =>
                "if" <+> parens (toParenDoc (c)) <+> toDoc (ts)

            case CIfElseStatement (c, ts, es) =>
                "if" <+> parens (toParenDoc (c)) <+> toDoc (ts) <+> "else" <+> toDoc (es)

            case CWhileStatement (c, ss) =>
                "while" <+> parens (toParenDoc (c)) <+> toDoc (ss)

            case _ =>
                super.toDoc (n)
        }

}
