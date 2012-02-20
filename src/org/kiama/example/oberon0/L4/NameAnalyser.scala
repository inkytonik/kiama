package org.kiama
package example.oberon0
package L4

trait NameAnalyser extends L3.NameAnalyser {

    import base.source.SourceASTNode
    import L0.source.Expression
    import org.kiama.util.Messaging.message
    import source.{FieldExp, IndexExp, RecordTypeDef}

    abstract override def check (n : SourceASTNode) {
        n match {
            case n @ RecordTypeDef (fls) =>
                val flnames = fls.flatMap (_.idndefs)
                if (flnames.distinct != flnames)
                    message (n, "record contains duplicate field names")

            case _ =>
                // Do nothing by default
        }

        super.check (n)
    }

    override def isLvalue (l : Expression) : Boolean =
        l match {
            case _ : IndexExp | _ : FieldExp =>
                true
            case _ =>
                super.isLvalue (l)
        }

}
