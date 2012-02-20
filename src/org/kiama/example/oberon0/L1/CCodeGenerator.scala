package org.kiama
package example.oberon0
package L1

/**
 * C Code generator for the L1 language.
 */
trait CCodeGenerator extends L0.CCodeGenerator {
    
    import base.c.CStatement
    import base.source.{Statement, Block}
    import c.{CIfElseStatement, CIfStatement, CWhileStatement}
    import L0.source.Expression
    import source.{IfStatement, WhileStatement}

    /**
     * Add translation of IF and WHILE statements.
     */    
    override def translate (s : Statement) : CStatement =
        s match {
            case IfStatement (c, ss, eis, oe) =>
                translate ((c,ss) :: eis, oe)

            case WhileStatement (c, b) =>
                CWhileStatement (translate (c), translate (b))
                
            case _ =>
                super.translate (s)
        }

    /**
     * Translation of expression, block pairs from an IF statement into
     * cascading C IFs.
     */
    def translate (eis : List[(Expression,Block)], oe : Option[Block]) : CStatement = {
        val (e, ss) = eis.last
        val te = translate (e)
        val tss = translate (ss)
        var s = oe.map (b => CIfElseStatement (te, tss, translate (b))).getOrElse (CIfStatement (te, tss))
        for ((e,ss) <- eis.init.reverse) {
            s = CIfElseStatement (translate (e), translate (ss), s) 
        }
        s
    }

}
