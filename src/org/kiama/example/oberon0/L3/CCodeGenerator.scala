package org.kiama
package example.oberon0
package L3

/**
 * C Code generator for the L3 language.
 */
trait CCodeGenerator extends L1.CCodeGenerator with TypeAnalyser {

    import base.c.{CBlock, CDeclaration, CExpression, CFunctionDecl,
        CInclude, CProgram, CStatement, CVarDecl}
    import base.source.{Block, Declaration, IdnDef, IdnUse, ModuleDecl,
        Statement}
    import c.{CAddrExp, CAddrType, CCall, CDerefExp, CStrExp, CVoidType}
    import L0.source.{Expression, IdnExp}
    import source.{Call, Mode, ProcDecl, ValMode, VarMode}

    /**
     * Add STDIO header since output is now possible.
     */
    override def translate (m : ModuleDecl) : CProgram = {
        val CProgram (is, ds) = super.translate (m)
        CProgram (CInclude ("<stdio.h>") :: is, ds)
    }

    /**
     * Add translation of procedure declarations.
     */
    override def translate (d : Declaration) : List[CDeclaration] =
        d match {
            case ProcDecl (p @ IdnDef (i), ps, Block (ds, ss), _) =>
                List (CFunctionDecl (CVarDecl (mangle (i), CVoidType ()),
                                     translateFormalParams (p),
                                     CBlock (ds flatMap translate,
                                             ss map translate)))
            case _ =>
                super.translate (d)
        }

    /**
     * Translate the formal parameters of a particular defined procedure.
     */
    def translateFormalParams (p : IdnDef): List[CDeclaration] =
        (p->parameters).get.map {
            case ParamInfo (m, i, t) =>
                translateFormalParam (m, i, t)
        }

    /**
     * Translate a formal parameter into a C parameter variable declaration
     * with the appropriate mode (address for Var, other value).
     */
    def translateFormalParam (m : Mode, i : String, t  : Type) : CDeclaration = {
        val tt = translate (t)
        CVarDecl (mangle (i), if (m == VarMode ()) CAddrType (tt) else tt)
    }

    /**
     * Add translation of call statements.
     */
    override def translate (s : Statement) : CStatement =
        s match {
            case Call (u @ IdnUse (s), ps) =>
                val cps = translateActualParams (u, ps)
                (u->entity) match {
                    case _ : BuiltinProc =>
                        s match {
                            case "Read" =>
                                CCall ("scanf", CStrExp ("%d") :: cps)
                            case "Write" =>
                                CCall ("printf", CStrExp (" %d") :: cps)
                            case "WriteLn" =>
                                CCall ("puts", List (CStrExp ("")))
                        }
                    case _ =>
                        CCall (mangle (s), cps)
                }
            case _ =>
                super.translate (s)
        }

    /**
     * Translate the actual parameters of a procedure call. Assumes that the
     * right number of parameters are present.
     */
    def translateActualParams (u : IdnUse, ps : List[Expression]) : List[CExpression] =
        (for (i <- 0 until ps.length)
            yield
                translateActualParam (ps (i), parammode (u, i + 1))
         ).toList

    /**
     * Compute an expression for a given actual parameter, inserting an
     * address-of operation for VAR parameters.
     */
    def translateActualParam (p : Expression, mode : Mode) : CExpression = {
        val cpsi = translate (p)
        mode match {
            case VarMode () =>
                cpsi match {
                    case CDerefExp (dcpsi) => dcpsi
                    case _                 => CAddrExp (cpsi)
                }
            case ValMode () =>
                cpsi
        }
    }

    /**
     * Add translation of uses of variable mode parameters in expressions
     * by dereferencing.
     */
    override def translate (e : Expression) : CExpression =
        e match {
            case IdnExp (u @ IdnUse (s)) =>
                val te = super.translate (e)
                (u->entity) match {
                    case Parameter (VarMode (), v) => CDerefExp (te)
                    case _                         => te
                }
            case _ =>
                super.translate (e)
        }

}
