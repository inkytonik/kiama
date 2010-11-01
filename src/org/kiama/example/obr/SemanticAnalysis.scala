/**
 * Semantic analysis for the Obr language.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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

object SemanticAnalysis {

    import ObrTree._
    import SymbolTable._
    import org.kiama.attribution.Attribution._
    import org.kiama.util.Messaging._

    /**
     * Does the sub-tree rooted at the given node contain any semantic
     * errors or not? If yes, as a side-effect this attribute will record
     * those errors using the Messaging module so that they can be
     * reported to the user later.
     */
    val errors : ObrNode ==> Unit =
        attr {
            case p @ ObrInt (i1, ds, ss, i2) =>
                // Apply errors to each child in turn and return true
                // if there is one that contains errors
                ds map (errors)
                ss map (errors)
                if (i1 != i2)
                    message (p, "identifier " + i2 + " at end should be " + i1)

            case n @ AssignStmt (l, r)  =>
                l->errors
                r->errors
                if (!(l->assignable))
                    message (l, "illegal assignment")

            case n @ ExitStmt () if (!(n->isinloop)) =>
                message (n, "an EXIT statement must be inside a LOOP statement")

            case n @ ForStmt (i, e1, e2, ss) =>
                e1->errors
                e2->errors
                ss map (errors)
                if (n->entity == Unknown)
                    message (n, i + " is not declared")
                val t = (n->entity).tipe;
                if ((t != IntType) && (t != UnknownType))
                    message (n, "for loop variable " + i + " must be integer")

            case IfStmt (e, ss1, ss2) =>
                e->errors
                ss1 map (errors)
                ss2 map (errors)

            case LoopStmt (ss) =>
                ss map (errors)

            case ReturnStmt (e) =>
                e->errors

            case WhileStmt (e, ss) =>
                e->errors
                ss map (errors)

            case n @ IntParam (i) if (n->entity == Multiple) =>
                message (n, i + " is declared more than once")

            case n @ IntVar (i) if (n->entity == Multiple) =>
                message (n, i + " is declared more than once")

            case n @ BoolVar (i) if (n->entity == Multiple) =>
                message (n, i + " is declared more than once")

            case n @ ArrayVar (i, v) if (n->entity == Multiple) =>
                message (n, i + " is declared more than once")

            case n @ RecordVar (i, _) =>
                (n->entity) match {
                     case Variable (RecordType (fs)) =>
                         if (fs.distinct.length != fs.length)
                             message (n, i + " contains duplicate field(s)")
                     case Multiple =>
                         message (n, i + " is declared more than once")
                }

            case n @ IntConst (i, v) if (n->entity == Multiple) =>
                message (n, i + " is declared more than once")

            case e : Expression =>
                e match {
                    case v @ IdnExp (i) if (v->entity == Unknown) =>
                        message (v, i + " is not declared")

                    case v @ IndexExp (a, r) =>
                        r->errors
                        (v->entity).tipe match {
                            case ArrayType (_) =>
                            case _ =>
                                message (v, "attempt to index the non-array " + a)
                        }

                    case v @ FieldExp (r, f) =>
                        ((v->entity).tipe) match {
                            case RecordType (fs) =>
                                if (! (fs contains f))
                                    message (v, f + " is not a field of " + r)
                            case _ =>
                                message (v, "attempt to access field of non-record " + r)
                        }

                    case AndExp (l, r)      => l->errors; r->errors
                    case EqualExp (l, r)    => l->errors; r->errors
                    case GreaterExp (l, r)  => l->errors; r->errors
                    case LessExp (l, r)     => l->errors; r->errors
                    case MinusExp (l, r)    => l->errors; r->errors
                    case ModExp (l, r)      => l->errors; r->errors
                    case NegExp (e)         => e->errors
                    case NotEqualExp (l, r) => l->errors; r->errors
                    case NotExp (e)         => e->errors
                    case OrExp (l, r)       => l->errors; r->errors
                    case PlusExp (l, r)     => l->errors; r->errors
                    case SlashExp (l, r)    => l->errors; r->errors
                    case StarExp (l, r)     => l->errors; r->errors
                    case _                  =>
                }
                if (!iscompatible (e->tipe, e->exptipe))
                    message (e, "type error: expected " + (e->exptipe) + " got " + (e->tipe))

            case _ =>

        }

    /**
     * Are two types compatible?  If either of them are unknown then we
     * assume an error has already been raised elsewhere so we say they
     * are compatible with anything.  Otherwise the two types have to be
     * the same.
     */
    def iscompatible (t1 : Type, t2 : Type) : Boolean =
        (t1 == UnknownType) || (t2 == UnknownType) || (t1 == t2)

    /**
     * The environment containing all bindings visible at a particular
     * node in the tree, not including any that are defined at that node.
     */
    val env : ObrNode ==> Environment =
        attr {
            case ObrInt (_, ds, ss, _)          => (ds.last)->envout
            case d : Declaration if (d.isFirst) => Map ()
            case d : Declaration                => (d.prev[Declaration])->envout
            case n                              => (n.parent[ObrNode])->env
        }

    /**
     * The environment containing all bindings visible "after" a
     * particular node in the tree.  I.e., its the environment at the
     * node plus any new bindings introduced by the node.
     */
    val envout : ObrNode ==> Environment =
        attr {
            case n @ IntParam (i)      => define (n, i, Variable (IntType))
            case n @ IntVar (i)        => define (n, i, Variable (IntType))
            case n @ BoolVar (i)       => define (n, i, Variable (BoolType))
            case n @ ArrayVar (i, v)   => define (n, i, Variable (ArrayType (v)))
            case n @ RecordVar (i, fs) => define (n, i, Variable (RecordType (fs)))
            case n @ IntConst (i, v)   => define (n, i, Constant (v))
            case n                     => n->env
        }

    /**
     * n is a node with an environment of bindings already seen.  Add a
     * binding of i to e and return the complete set of bindings, unless
     * i already has a binding at n, in which case define i to be an
     * multiply-defined entity.
     */
    def define (n : ObrNode, i : Identifier, e : => Entity) : Environment =
        if (n->env contains i)
            (n->env) + ((i, Multiple))
        else
            (n->env) + ((i, e))

    /**
     * The entity referred to by a declaration or a variable expression.
     * If a name has been used previously in a declaration then return an
     * unknown entity which will trigger an error.
     */
    val entity : EntityNode ==> Entity =
        attr {
            case n @ IntParam (i)     => (n->envout) (i)
            case n @ IntVar (i)       => (n->envout) (i)
            case n @ BoolVar (i)      => (n->envout) (i)
            case n @ ArrayVar (i, v)  => (n->envout) (i)
            case n @ RecordVar (i, _) => (n->envout) (i)
            case n @ IntConst (i, v)  => (n->envout) (i)

            case n @ ForStmt (i, e1, e2, ss) =>
                (n->env).get (i) match {
                     case Some (e) => e
                     case None     => Unknown
                }

            case n @ IdnExp (i) =>
                (n->env).get (i) match {
                     case Some (e) => e
                     case None     => Unknown
                }
            case n @ IndexExp (i, _) =>
                (n->env).get (i) match {
                     case Some (e) => e
                     case None     => Unknown
                }
            case n @ FieldExp (i, _) =>
                (n->env).get (i) match {
                     case Some (e) => e
                     case None     => Unknown
                }
        }

    /**
     * What is the type of an expression?
     */
    val tipe : Expression ==> Type =
        attr {
            case AndExp (l, r)      => BoolType
            case BoolExp (b)        => BoolType
            case EqualExp (l, r)    => BoolType
            case FieldExp (r, f)    => IntType
            case GreaterExp (l, r)  => BoolType
            case n : IdnExp         => (n->entity).tipe
            case IndexExp (l, r)    => IntType
            case IntExp (i)         => IntType
            case LessExp (l, r)     => BoolType
            case MinusExp (l, r)    => IntType
            case ModExp (l, r)      => IntType
            case NegExp (e)         => IntType
            case NotEqualExp (l, r) => BoolType
            case NotExp (e)         => BoolType
            case OrExp (l, r)       => BoolType
            case PlusExp (l, r)     => IntType
            case SlashExp (l, r)    => IntType
            case StarExp (l, r)     => IntType
        }

    /**
     * What is the expected type of an expression?  I.e., what type does
     * the context impose on it.  Returns UnknownType if any type will do.
     */
    val exptipe : Expression ==> Type =
        attr {
            case e =>
                (e.parent) match {
                    case AssignStmt (IndexExp (_, _), `e`) => IntType
                    case AssignStmt (FieldExp (_, _), `e`) => IntType
                    case AssignStmt (v : IdnExp, `e`)      => (v->entity).tipe

                    case ForStmt (_, _, _, _) => IntType
                    case IfStmt (_, _, _)     => BoolType
                    case ReturnStmt (_)       => IntType
                    case WhileStmt (_, _)     => BoolType

                    case AndExp (_, _)        => BoolType
                    case EqualExp (l, `e`)    => l->tipe
                    case GreaterExp (_, _)    => IntType
                    case IndexExp (_, `e`)    => IntType
                    case LessExp (_, _)       => IntType
                    case MinusExp (_, _)      => IntType
                    case ModExp (_, _)        => IntType
                    case NegExp (_)           => IntType
                    case NotEqualExp (l, `e`) => l->tipe
                    case NotExp (_)           => BoolType
                    case OrExp (_, _)         => BoolType
                    case PlusExp (_, _)       => IntType
                    case SlashExp (_, _)      => IntType
                    case StarExp (_, _)       => IntType

                    case _                    => UnknownType
                }
        }

    /**
     * Is the expression something that can be assigned to?
     */
    val assignable : Expression ==> Boolean =
        attr {
            case n @ IdnExp (_)  => (n->entity).isassignable
            case IndexExp (_, _) => true
            case FieldExp (_, _) => true
            case _               => false
        }

    /**
     * Is this statement inside a LOOP statement?  Used to
     * check that EXIT statements are placed appropriately.
     */
    val isinloop : Statement ==> Boolean =
        attr {
            case s => (s.parent) match {
                case _ : ObrInt    => false
                case LoopStmt (_)  => true
                case p : Statement => p->isinloop
            }
        }

}
