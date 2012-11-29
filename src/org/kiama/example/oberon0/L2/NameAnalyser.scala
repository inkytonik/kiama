package org.kiama
package example.oberon0
package L2

trait NameAnalyser extends L0.NameAnalyser {

    import L0.source.Expression
    import org.kiama.util.Messaging.message
    import org.kiama.util.Patterns.HasParent
    import source.{ForStatement, MinMaxCond, ValCond}

    override def rootconstexpDef : Expression => Boolean =
        {
            case HasParent (e1, ForStatement (_, _, _, Some (e2), _)) if e1 == e2 =>
                true
            case HasParent (_, _ : ValCond | _ : MinMaxCond) =>
                true
            case e =>
                super.rootconstexpDef (e)
        }

}
