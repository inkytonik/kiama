/**
 * Semantic analysis for the Obr language.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2011 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2011 Dominic Verity, Macquarie University.
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
    import org.kiama.attribution.Attributable
    import org.kiama.attribution.Attribution._
    import org.kiama.util.Messaging._

    /**
     * Visit all nodes to check for semantic errors.  Errors will be recorded
     * using the Messaging module so that they can be reported to the user later.
     */
    val errors : Attributable => Unit =
        attr {
            case node =>
                // Process the errors of the children of node
                for (child <- node.children)
                    child->errors
                    
                node match {
            
                    case p @ ObrInt (i1, ds, ss, i2) =>
                        if (i1 != i2)
                            message (p, "identifier " + i2 + " at end should be " + i1)
                    
                    case n @ AssignStmt (l, r)  =>
                        if (!(l->assignable))
                            message (l, "illegal assignment")
                    
                    case n @ ExitStmt () if (!(n->isinloop)) =>
                        message (n, "an EXIT statement must be inside a LOOP statement")
                    
                    case n @ ForStmt (i, e1, e2, ss) =>
                        if (n->entity == Unknown)
                            message (n, i + " is not declared")
                        val t = (n->entity).tipe;
                        if ((t != IntType) && (t != UnknownType))
                            message (n, "for loop variable " + i + " must be integer")
                    
                    // Check a RAISE statement to make sure its parameter is an exception constant.
                    case n @ RaiseStmt (i) =>
                        if (n->entity == Unknown)
                            message (n, i + " is not declared")
                        val t = (n->entity).tipe;
                        if ((t != ExnType) && (t != UnknownType))
                            message (n, "raise parameter " + i + " must be an exception constant")
                    
                    // Check a CATCH clause to make sure its parameter is an exception constant.
                    case n @ Catch (i, ss) =>
                        if (n->entity == Unknown)
                            message (n, i + " is not declared")
                        val t = (n->entity).tipe;
                        if ((t != ExnType) && (t != UnknownType))
                            message (n, "catch clause parameter " + i + " must be an exception constant")
                                        
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
                    
                    // Extra clauses to report errors from enumeration variable declarations
                    case n @ EnumVar (i, cs)                            =>
                        if (n->entity == Multiple)
                            message (n, i + " is declared more than once")
                    
                    case n @ EnumConst (i) if (n->entity == Multiple)   =>
                        message (n, i + " is declared more than once")
                    
                    // Extra clause to report errors from exception constant declarations
                    case n @ ExnConst (i) if (n->entity == Multiple)    =>
                        message (n, i + " is declared more than once")
                    
                    case e : Expression =>
                        e match {
                            case v @ IdnExp (i) if (v->entity == Unknown) =>
                                message (v, i + " is not declared")
                    
                            case v @ IndexExp (a, r) =>
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
                    
                            case _                  =>
                        }
                        if (!(e->exptipe exists ((_ : TypeBase) iscompatible e->tipe)))
                            message (e, "type error: expected " + (e->exptipe).mkString(" or ") +
                                     " got " + (e->tipe))
                    
                    case _ =>

                }
        }

    /**
     * Attribute to consecutively number enumeration constants.
     */
    val enumconstnum : EnumConst => Int =
        attr {
            case c if (c.isFirst)   => 0
            case c                  => (c.prev[EnumConst]->enumconstnum) + 1
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
            case c if (c.isFirst)   => userExn
            case c                  =>
                c.prev[Declaration] match {
                    case d : ExnConst   => (d->exnconstnum) + 1
                    case d              => d->exnconstnum
                }
        }

    /**
     * Initial environment, pre-primed with predeclared identifiers
     * like DivideByZero
     */
    val initEnv = Map (
            "DivideByZero" -> Constant (ExnType, divideByZeroExn)
        ,   "IndexOutOfBounds" -> Constant (ExnType, indexOutOfBoundsExn)
        )

    /**
     * The environment containing all bindings visible at a particular
     * node in the tree, not including any that are defined at that node.
     */
    val env : ObrNode => Environment =
        attr {
            case ObrInt (_, ds, ss, _)          => (ds.last)->envout
            case d : Declaration if (d.isFirst) => initEnv
            case d : Declaration                => (d.prev[Declaration])->envout
            case d : EnumConst if (!d.isFirst)  => (d.prev[EnumConst])->envout
            case n                              => (n.parent[ObrNode])->env
        }

    /**
     * The environment containing all bindings visible "after" a
     * particular node in the tree.  I.e., its the environment at the
     * node plus any new bindings introduced by the node.
     */
    val envout : ObrNode => Environment =
        attr {
            case n @ IntParam (i)      => define (n->env, i, Variable (IntType))
            case n @ IntVar (i)        => define (n->env, i, Variable (IntType))
            case n @ BoolVar (i)       => define (n->env, i, Variable (BoolType))
            case n @ ArrayVar (i, v)   => define (n->env, i, Variable (ArrayType (v)))
            case n @ RecordVar (i, fs) => define (n->env, i, Variable (RecordType (fs)))
            // Extra clauses for the defining instance of an enumeration variable...
            case n @ EnumVar (i, _)    =>
                define (n.lastChild[EnumConst]->envout, i, Variable (EnumType (i)))
            // ... and its constants
            case n @ EnumConst (i)     =>
                val EnumVar (pi, _) = n.parent[EnumVar]
                define (n->env, i, Constant (EnumType (pi), n->enumconstnum))
            // Extra clause for exception constants
            case n @ ExnConst (i)      => define (n->env, i, Constant (ExnType, n->exnconstnum))
            case n @ IntConst (i, v)   => define (n->env, i, Constant (IntType, v))
            case n                     => n->env
        }

    /**
     * envin is an environment of bindings already seen.  Add a binding of i
     * to e and return the complete set of bindings, unless i already has a
     * binding in envin, in which case define i to be a multiply-defined entity.
     */
    def define (envin : Environment, i : Identifier, e : => Entity) : Environment =
        if (envin contains i)
            envin + ((i, Multiple))
        else
            envin + ((i, e))

    /**
     * The entity referred to by a declaration or a variable expression.
     * If a name has been used previously in a declaration then return an
     * unknown entity which will trigger an error.
     */
    val entity : EntityNode => Entity =
        attr {
            case n @ IntParam (i)     => (n->envout) (i)
            case n @ IntVar (i)       => (n->envout) (i)
            case n @ BoolVar (i)      => (n->envout) (i)
            case n @ ArrayVar (i, v)  => (n->envout) (i)
            case n @ RecordVar (i, _) => (n->envout) (i)
            // Extra clauses to lookup entities for enumeration variable / constant declarations
            case n @ EnumVar (i, _)   => (n->envout) (i)
            case n @ EnumConst (i)    => (n->envout) (i)
            // Extra clause to lookup entity for an exception constant declaration
            case n @ ExnConst (i)     => (n->envout) (i)
            case n @ IntConst (i, v)  => (n->envout) (i)

            case n @ ForStmt (i, e1, e2, ss) =>
                (n->env).get (i) match {
                     case Some (e) => e
                     case None     => Unknown
                }

            // Extra clause to lookup entity for the exception in a RAISE statement
            case n @ RaiseStmt (i)    =>
                (n->env).get (i) match {
                    case Some (e) => e
                    case None     => Unknown
                }

            // Extra clause to lookup entity for the exception in a CATCH clause
            case n @ Catch (i, _)     =>
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
    val tipe : Expression => Type =
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
    val exptipe : Expression => Set[TypeBase] =
        attr {
            case e =>
                (e.parent) match {
                    case AssignStmt (IndexExp (_, _), e1) if (e eq e1)  => Set (IntType)
                    case AssignStmt (FieldExp (_, _), e1) if (e eq e1)  => Set (IntType)
                    case AssignStmt (v : IdnExp, e1) if (e eq e1)       => Set ((v->entity).tipe)

                    case ForStmt (_, _, _, _)                           => Set (IntType)
                    case IfStmt (_, _, _)                               => Set (BoolType)
                    case ReturnStmt (_)                                 => Set (IntType)
                    case WhileStmt (_, _)                               => Set (BoolType)

                    case AndExp (_, _)                                  => Set (BoolType)
                    case EqualExp (l, e1) if (e eq e1)                  => Set (l->tipe)

                    // The left operand of a GreaterExp must be an integer or an enumeration value
                    case GreaterExp (e1, _) if (e eq e1)                => Set (IntType, EnumTypes)
                    // The left and right operands of a GreaterExp must have the same type
                    case GreaterExp (l, e1) if (e eq e1)                =>
                        if ((l->tipe == IntType) || ((l->tipe).isInstanceOf[EnumType]))
                            Set (l->tipe)
                        else
                            Set (UnknownType)

                    case IndexExp (_, e1) if (e eq e1)                  => Set (IntType)

                    // The left operand of a LessExp must be an integer or an enumeration value
                    case LessExp (e1, _) if (e eq e1)                   => Set (IntType, EnumTypes)
                    // The left and right operands of a LessExp must have the same type
                    case LessExp (l, e1) if (e eq e1)                   =>
                        if ((l->tipe == IntType) || ((l->tipe).isInstanceOf[EnumType]))
                            Set (l->tipe)
                        else
                            Set (UnknownType)

                    case MinusExp (_, _)                                => Set (IntType)
                    case ModExp (_, _)                                  => Set (IntType)
                    case NegExp (_)                                     => Set (IntType)
                    case NotEqualExp (l, e1) if (e eq e1)               => Set (l->tipe)
                    case NotExp (_)                                     => Set (BoolType)
                    case OrExp (_, _)                                   => Set (BoolType)
                    case PlusExp (_, _)                                 => Set (IntType)
                    case SlashExp (_, _)                                => Set (IntType)
                    case StarExp (_, _)                                 => Set (IntType)

                    case _                                              => Set (UnknownType)
                }
        }

    /**
     * Is the expression something that can be assigned to?
     */
    val assignable : Expression => Boolean =
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
    val isinloop : Statement => Boolean =
        attr {
            case s => (s.parent) match {
                case _ : ObrInt    => false
                case LoopStmt (_)  => true
                case p : Statement => p->isinloop
            }
        }

}
