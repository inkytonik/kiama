package org.kiama
package example.oberon0
package L2

trait TypeAnalyser extends L1.TypeAnalyser {

    import base.source.{IdnUse, SourceASTNode}
    import L0.source.{Expression, IdnExp}
    import org.kiama.util.Messaging.message
    import source.{CaseStatement, ForStatement}

    abstract override def check (n : SourceASTNode) {
        n match {
            case ForStatement (IdnExp (u @ IdnUse (i)), _, _, _, _) if !isVariable (u->entity) =>
                message (u, "illegal FOR loop control variable " + i)

            case _ =>
                // Do nothing by default
        }

        super.check (n)
    }

    override def exptypeDef : Expression => Type =
        {
            case n =>
                n.parent match {
                    case _ : ForStatement | _ : CaseStatement =>
                        integerType

                    case _ =>
                        super.exptypeDef (n)
                }
        }

}
