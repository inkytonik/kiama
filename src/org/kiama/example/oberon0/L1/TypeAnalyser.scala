package org.kiama
package example.oberon0
package L1

trait TypeAnalyser extends L0.TypeAnalyser {
    
    import L0.source.Expression
    import org.kiama.attribution.Attribution.attr
    import source.{IfStatement, WhileStatement}
    
    /**
     * The type expected of an expression as defined by its context.
     */
    override def exptypeDef : Expression => Type =
        {
            case n =>
                n.parent match {
                    case _ : IfStatement | _ : WhileStatement =>
                        booleanType
                        
                    case _ =>
                        super.exptypeDef (n)
                }
        }
    
}
