package org.kiama
package example.oberon0
package L3

trait SymbolTable extends L0.SymbolTable {

    import source.{Mode, ProcDecl, ValMode, VarMode}

    /**
     * A procedure entity represented by a procedure declaration.
     */
    case class Procedure (ident : String, decl : ProcDecl) extends NamedEntity

    /**
     * A parameter is a variable augmented with a passing mode.
     */
    case class Parameter (mode : Mode, varr : Variable) extends NamedEntity {
        def ident : String = varr.ident
    }

    /**
     * Parameters are variables too.
     */
    override def isVariable (e : Entity) : Boolean =
        super.isVariable (e) || e.isInstanceOf[Parameter]

    /**
     * Information about a particular parameter.  Similar to Parameter but the type
     * has been replaced with its definition.
     */
    case class ParamInfo (mode : Mode, ident : String, tipe : Type)

    /**
     * A built-in procedure with its parameter information.
     */
    case class BuiltinProc (ident : String, params : List[ParamInfo]) extends NamedEntity with Builtin

    /**
     * The built-in Read procedure.
     */
    lazy val readProc =
        BuiltinProc ("Read", List (ParamInfo (VarMode (), "ReadParam", integerType)))

    /**
     * The built-in Write procedure.
     */
    lazy val writeProc =
        BuiltinProc ("Write", List (ParamInfo (ValMode (), "WriteParam", integerType)))

    /**
     * The built-in WriteLn procedure.
     */
    lazy val writelnProc =
        BuiltinProc ("WriteLn", Nil)

    /**
     * The default environment with pre-defined procedures added.
     */
    override def defenvPairs : List[(String,Entity)] =
        super.defenvPairs :::
        List (
            "Read"    -> readProc,
            "Write"   -> writeProc,
            "WriteLn" -> writelnProc
        )

}
