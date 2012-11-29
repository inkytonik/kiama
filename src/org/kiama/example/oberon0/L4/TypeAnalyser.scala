package org.kiama
package example.oberon0
package L4

trait TypeAnalyser extends L3.TypeAnalyser with SymbolTable {

    import base.source.{IdnDef, SourceASTNode}
    import L0.source.{Assignment, Expression, NamedType, TypeDef}
    import L3.source.{FPSection, ValMode}
    import org.kiama.attribution.Attribution.attr
    import org.kiama.util.Messaging.message
    import org.kiama.util.Patterns.HasParent
    import source.{ArrayTypeDef, FieldExp, FieldIdn, FieldList, IndexExp,
        RecordTypeDef}

    abstract override def check (n : SourceASTNode) {
        n match {
            case FPSection (_, _, t) if !t.isInstanceOf[NamedType] =>
                message (t, "parameter type must be identifier")

            case FPSection (ValMode (), _, t) if !(isNotArray (typebasetype (t->deftype))) =>
                message (n, "array parameter must be VAR")

            case FPSection (ValMode (), _, t) if !(isNotRecord (typebasetype (t->deftype))) =>
                message (n, "record parameter must be VAR")

            case ArrayTypeDef (s, t) =>
                if (isInteger (s->tipe) && (s->isconst) && (s->value < 0))
                    message (s, "ARRAY size is " + (s->value) + " but should be >= 0")

            case Assignment (l, _) if !(isNotArray (l->basetype)) =>
                message (n, "can't assign to array")

            case IndexExp (a, _) if !(isArray (a->basetype)) =>
                message (a, "array indexing attempted on non-ARRAY")

            case IndexExp (a, e) =>
                val ArrayType (s, _) = a->basetype
                if ((e->basetype == integerType) && (e->isconst) && ((e->value < 0) || (e->value >= s)))
                    message (e, "index out of range")

            case Assignment (l, _) if !(isNotRecord (l->basetype))  =>
                message (n, "can't assign to record")

            case FieldExp (r, f @ FieldIdn (i)) =>
                val t = r->basetype
                if (isRecord (t)) {
                    if (!hasField (t, i))
                        message (f, "record doesn't contain a field called '" + i + "'")
                } else
                    message (f, "field access attempted on non-RECORD")

            case _ =>
                // Do nothing by default
        }

        super.check (n)
    }

    override def rootconstexpDef : Expression => Boolean =
        {
            case HasParent (_, _ : ArrayTypeDef) =>
                true
            case e =>
                super.rootconstexpDef (e)
        }

    /**
     * Array and record types are only compatible if they have the same name.
     */
    override def isCompatible (tipe : Type, exptype : Type) : Boolean =
        (tipe, exptype) match {
            case (UserType (n1, _), UserType (n2, _)) =>
                n1 == n2
            case _ =>
                super.isCompatible (tipe, exptype)
        }

    override def entityFromDecl (n : IdnDef, i : String) : Entity =
        n.parent match {
            case FieldList (_, t) => Field (i, t->deftype)
            case _                => super.entityFromDecl (n, i)
        }

    override def deftypeDef : TypeDef => Type =
        {
            case ArrayTypeDef (s, t) =>
                ArrayType (s->value, t->deftype)
            case RecordTypeDef (fls) =>
                RecordType (fieldListsToFields (fls))
            case n =>
                super.deftypeDef (n)
        }

    def fieldListsToFields (fls : List[FieldList]) : List[Field] =
        (for (fl <- fls)
            yield {
                val t = (fl.tipe)->deftype
                fl.idndefs.map { case f => Field (f, t) }
            }).flatten

    override def tipeDef : Expression => Type =
        {
            case IndexExp (a, _) =>
                a->basetype match {
                    case ArrayType (_, t) => t
                    case _                   => unknownType
                }

            case f @ FieldExp (r, FieldIdn (i)) =>
                r->basetype match {
                    case RecordType (fs) =>
                        fs.filter (_.ident == i) match {
                            case List (f) => f.tipe
                            case _        => unknownType
                        }
                    case _ =>
                        unknownType
                }

            case n =>
                super.tipeDef (n)
        }

    /**
     * Use of arrays and records is dealt with separately, not via the
     * expected type.
     */
    override def exptypeDef : Expression => Type =
        {
            case n =>
                n.parent match {
                    case IndexExp (a, e) if a eq n     => unknownType
                    case IndexExp (_, e) if e eq n     => integerType
                    case p @ FieldExp (r, f) if r eq n => unknownType
                    case _                             => super.exptypeDef (n)
                }
        }

}
