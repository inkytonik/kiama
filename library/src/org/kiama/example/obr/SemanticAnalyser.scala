/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2014 Dominic Verity, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.kiama
package example.obr

class SemanticAnalyser {

    import ObrTree._
    import SymbolTable._
    import org.kiama.attribution.Attribution._
    import org.kiama.attribution.Decorators.{chain, Chain}
    import org.kiama.util.{Entity, MultipleEntity, UnknownEntity}
    import org.kiama.util.Message
    import org.kiama.util.Messaging.{check, checkuse, collectmessages, Messages, message, noMessages}
    import scala.collection.immutable.Seq

    /**
     * The semantic error messages for a given tree.
     */
    val errors : ObrTree => Messages =
        attr { collectmessages {
            case p @ ObrInt (i1, ds, ss, i2) if i1 != i2 =>
                message (p, s"identifier $i2 at end should be $i1")

            case d @ IdnDef (i) if d->entity == MultipleEntity () =>
                message (d, s"$i is declared more than once")

            case u @ IdnUse (i) if u->entity == UnknownEntity () =>
                message (u, s"$i is not declared")

            case n @ AssignStmt (l, r) if !(l->assignable) =>
                message (l, "illegal assignment")

            case n @ ExitStmt () if !(n->isinloop) =>
                message (n, "an EXIT statement must be inside a LOOP statement")

            case ForStmt (n @ IdnUse (i), e1, e2, ss) =>
                checkuse (n->entity) {
                    case ent =>
                        val t = enttipe (ent)
                        message (n, s"for loop variable $i must be integer",
                                 (t != IntType ()) && (t != UnknownType ()))
                }

            // Check a RAISE statement to make sure its parameter is an exception constant.
            case n @ RaiseStmt (v @ IdnUse (i)) =>
                checkuse (v->entity) {
                    case ent =>
                        val t = enttipe (ent)
                        message (n, s"raise parameter $i must be an exception constant",
                                 (t != ExnType ()) && (t != UnknownType ()))
                }

            // Check a CATCH clause to make sure its parameter is an exception constant.
            case n @ Catch (v @ IdnUse (i), ss) =>
                checkuse (v->entity) {
                    case ent =>
                        val t = enttipe (ent)
                        message (n, s"catch clause parameter $i must be an exception constant",
                                 (t != ExnType ()) && (t != UnknownType ()))
                }

            case RecordVar (n @ IdnDef (i), _) =>
                checkuse (n->entity) {
                     case Variable (RecordType (fs)) =>
                         message (n, s"$i contains duplicate field(s)",
                                  fs.distinct.length != fs.length)
                }

            case e : Expression =>
                check (e) {
                    case IndexExp (v @ IdnUse (a), r) =>
                        check (enttipe (v->entity)) {
                            case ArrayType (_) | UnknownType () =>
                                noMessages
                            case _ =>
                                message (v, s"attempt to index the non-array $a")
                        }

                    case FieldExp (v @ IdnUse (r), f) =>
                        check (enttipe (v->entity)) {
                            case RecordType (fs) =>
                                message (v, s"$f is not a field of $r", ! (fs contains f))
                            case _ =>
                                message (v, s"attempt to access field of non-record $r")
                        }
                } ++
                message (e, s"""type error: expected ${(e->exptipe).mkString(" or ")} got ${e->tipe}""",
                         ! (e->exptipe exists ((_ : TypeBase) iscompatible e->tipe)))
        }}

    /**
     * Attribute to consecutively number enumeration constants.
     */
    val enumconstnum : EnumConst => Int =
        attr {
            case c if c.index == 1 => 0
            case c                 => (c.prev[EnumConst]->enumconstnum) + 1
        }

    /**
     * Pre-defined exception numbers
     */
    val divideByZeroExn : Int = 0
    val indexOutOfBoundsExn : Int = 1
    val userExn : Int = 2

    /**
     * Attribute to consecutively number exception constants
     */
    val exnconstnum : Declaration => Int =
        attr {
            case c if c.isFirst => userExn
            case c =>
                c.prev[Declaration] match {
                    case d : ExnConst   => (d->exnconstnum) + 1
                    case d              => d->exnconstnum
                }
        }

    /**
     * Initial environment, primed with bindings for pre-defined identifiers.
     */
    val initenv =
        rootenv (
            "DivideByZero" -> Constant (ExnType (), divideByZeroExn),
            "IndexOutOfBounds" -> Constant (ExnType (), indexOutOfBoundsExn)
        )

    /**
     * The entity defined by a defining occurrence of an identifier.
     * Defined by the context of the occurrence.
     */
    lazy val defentity : IdnDef => Entity =
        attr {
            case n =>
                n.parent match {
                    case _ : IntParam | _ : IntVar =>
                        Variable (IntType ())
                    case _ : BoolVar =>
                         Variable (BoolType ())
                    case ArrayVar (_, v) =>
                        Variable (ArrayType (v))
                    case RecordVar (IdnDef (i), fs) =>
                        Variable (RecordType (fs))
                    case EnumVar (IdnDef (i), _) =>
                        Variable (EnumType (i))
                    case p @ EnumConst (IdnDef (i)) =>
                        val EnumVar (IdnDef (pi), _) = p.parent[EnumVar]
                        Constant (EnumType (pi), p->enumconstnum)
                    case p : ExnConst =>
                        Constant (ExnType (), p->exnconstnum)
                    case IntConst (_, v) =>
                        Constant (IntType (), v)
                    case _ =>
                        UnknownEntity ()
                }
        }

    /**
     * The environment containing bindings for things that are being defined.
     */
    lazy val defenv : Chain[ObrTree,Environment] =
        chain (defenvin, defenvout)

    def defenvin (in : ObrTree => Environment) : ObrTree ==> Environment = {

        // At the root, get the initial environment
        case n : ObrInt =>
            initenv

    }

    def defenvout (out : ObrTree => Environment) : ObrTree ==> Environment = {

        // At a defining occurrence of an identifier, check to see if it's already
        // been defined in this scope. If so, change its entity to MultipleEntity,
        // otherwise use the entity appropriate for this definition.
        case n @ IdnDef (i) =>
            val entity =
                if (isDefinedInScope (n->(defenv.in), i))
                    MultipleEntity ()
                else
                    n->defentity
            define (out (n), i, entity)

    }

    /**
     * The environment to use to lookup names at a node.
     */
    lazy val env : ObrTree => Environment =
        attr {

            // At a scope-introducing node, get the final value of the
            // defining environment, so that all of the definitions of
            // that scope are present.
            case n : ObrInt =>
                (n.lastChild[ObrTree])->defenv

            // Otherwise, ask our parent so we work out way up to the
            // nearest scope node ancestor (which represents the smallest
            // enclosing scope).
            case n =>
                (n.parent[ObrTree])->env

        }

    /**
     * The program entity referred to by an identifier definition or use.
     */
    lazy val entity : IdnTree => Entity =
        attr {

            // Just look the identifier up in the environment at the node.
            // Return `UnknownEntity` if the identifier is not defined.
            case n =>
                lookup (n->env, n.idn, UnknownEntity ())

        }

    /**
     * What is the type of an expression?
     */
    val tipe : Expression => Type =
        attr {
            case AndExp (l, r)      => BoolType ()
            case BoolExp (b)        => BoolType ()
            case EqualExp (l, r)    => BoolType ()
            case FieldExp (r, f)    => IntType ()
            case GreaterExp (l, r)  => BoolType ()
            case IdnExp (n)         => enttipe (n->entity)
            case IndexExp (l, r)    => IntType ()
            case IntExp (i)         => IntType ()
            case LessExp (l, r)     => BoolType ()
            case MinusExp (l, r)    => IntType ()
            case ModExp (l, r)      => IntType ()
            case NegExp (e)         => IntType ()
            case NotEqualExp (l, r) => BoolType ()
            case NotExp (e)         => BoolType ()
            case OrExp (l, r)       => BoolType ()
            case PlusExp (l, r)     => IntType ()
            case SlashExp (l, r)    => IntType ()
            case StarExp (l, r)     => IntType ()
        }

    /**
     * What is the expected type of an expression?  I.e., what type does
     * the context impose on it.  Returns UnknownType () if any type will do.
     */
    val exptipe : Expression => Set[TypeBase] =
        attr (
            e =>
                (e.parent) match {
                    case AssignStmt (IndexExp (_, _), e1) if e eq e1    => Set (IntType ())
                    case AssignStmt (FieldExp (_, _), e1) if e eq e1    => Set (IntType ())
                    case AssignStmt (IdnExp (v), e1) if e eq e1         => Set (enttipe (v->entity))

                    case ForStmt (_, _, _, _)                           => Set (IntType ())
                    case IfStmt (_, _, _)                               => Set (BoolType ())
                    case ReturnStmt (_)                                 => Set (IntType ())
                    case WhileStmt (_, _)                               => Set (BoolType ())

                    case AndExp (_, _)                                  => Set (BoolType ())
                    case EqualExp (l, e1) if e eq e1                    => Set (l->tipe)

                    // The left operand of a GreaterExp must be an integer or an enumeration value
                    case GreaterExp (e1, _) if e eq e1                  => Set (IntType (), EnumTypes ())
                    // The left and right operands of a GreaterExp must have the same type
                    case GreaterExp (l, e1) if e eq e1                  =>
                        if ((l->tipe == IntType ()) || ((l->tipe).isInstanceOf[EnumType]))
                            Set (l->tipe)
                        else
                            Set (UnknownType ())

                    case IndexExp (_, e1) if e eq e1                    => Set (IntType ())

                    // The left operand of a LessExp must be an integer or an enumeration value
                    case LessExp (e1, _) if e eq e1                     => Set (IntType (), EnumTypes ())
                    // The left and right operands of a LessExp must have the same type
                    case LessExp (l, e1) if e eq e1                     =>
                        if ((l->tipe == IntType ()) || ((l->tipe).isInstanceOf[EnumType]))
                            Set (l->tipe)
                        else
                            Set (UnknownType ())

                    case MinusExp (_, _)                                => Set (IntType ())
                    case ModExp (_, _)                                  => Set (IntType ())
                    case NegExp (_)                                     => Set (IntType ())
                    case NotEqualExp (l, e1) if e eq e1                 => Set (l->tipe)
                    case NotExp (_)                                     => Set (BoolType ())
                    case OrExp (_, _)                                   => Set (BoolType ())
                    case PlusExp (_, _)                                 => Set (IntType ())
                    case SlashExp (_, _)                                => Set (IntType ())
                    case StarExp (_, _)                                 => Set (IntType ())

                    case _                                              => Set (UnknownType ())
                }
        )

    /**
     * Is the expression something that can be assigned to?
     */
    val assignable : Expression => Boolean =
        attr {
            case IdnExp (n)      => isassignable (n->entity)
            case IndexExp (_, _) => true
            case FieldExp (_, _) => true
            case _               => false
        }

    /**
     * Is the entity assignable?
     */
    val isassignable : Entity => Boolean =
        attr {
            case _ : Constant => false
            case _            => true
        }

    /**
     * Is this statement inside a LOOP statement?  Used to
     * check that EXIT statements are placed appropriately.
     */
    val isinloop : Statement => Boolean =
        attr (
            s => (s.parent) match {
                case _ : ObrInt    => false
                case LoopStmt (_)  => true
                case p : Statement => p->isinloop
            }
        )

    /**
     * The type of an entity.
     */
    val enttipe : Entity => Type =
        attr {
            case Variable (tipe)    => tipe
            case Constant (tipe, _) => tipe
            case _                  => UnknownType ()
        }

    /**
     * Is an entity constant or not?
     */
    val isconst : Entity => Boolean =
        attr {
            case _ : Variable => false
            case _            => true
        }

}
