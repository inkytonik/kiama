/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012-2013 Anthony M Sloane, Macquarie University.
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
package example.minijava

import org.kiama.util.Environments

/**
 * Semantic analysis module containing static checking of Minijava
 * semantic rules, most notably name analysis.
 */
object SemanticAnalysis {

    import MiniJavaTree._
    import org.kiama.==>
    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators.{chain, Chain}
    import org.kiama.util.Patterns.HasParent
    import org.kiama.util.Messaging.message
    import SymbolTable._

    /**
     * Check the sub-tree rooted at the given node to see if it contains
     * any semantic errors. If yes, as a side-effect this method will
     * record those errors using the Messaging module so that they can be
     * reported to the user later.
     */
    def check (n : SourceNode) {
        // Check this node
        n match {
            case d @ IdnDef (i) if (d->entity == MultipleEntity ()) =>
                message (d, i + " is declared more than once")

            case u @ IdnUse (i) if (u->entity == UnknownEntity ()) =>
                message (u, i + " is not declared")

            case VarAssign (u, _) =>
                (u->entity) match {
                    // Rule 9
                    case _ : FieldEntity | _ : VariableEntity | _ : ArgumentEntity =>
                        // Do nothing
                    case ent =>
                        if (ent != UnknownEntity ())
                            message (u, "illegal assignment to non-variable, non-argument")
                }

            case e : Expression =>
                if (!iscompatible (e->tipe, e->exptipe))
                    message (e, "type error: expected " + (e->exptipe) +
                                    " got " + (e->tipe))
                e match {

                    case IdnExp (u) =>
                        (u->entity) match {
                            // Rule 6
                            case _ : MethodEntity =>
                                message (u, "can't refer to methods directly")
                            case _ =>
                                // Do nothing
                        }

                    case CallExp (_, u, args) =>
                        (u->entity) match {
                            // Rule 16
                            case MethodEntity (decl) =>

                                // Check that argument counts match (rule 16)
                                val expargnum = decl.body.args.length
                                if (expargnum != args.length)
                                    message (u, "wrong number of arguments, got " +
                                                 args.length + " but expected " +
                                                 expargnum)

                            case ent =>
                                if (ent != UnknownEntity ())
                                    message (u, "illegal call to non-method")
                        }

                    case NewExp (u) =>
                        (u->entity) match {
                            // Rule 19
                            case _ : ClassEntity =>
                                // Do nothing
                            case ent =>
                                if (ent != UnknownEntity ())
                                    message (u, "illegal instance creation of non-class type")
                        }

                    case _ =>
                        // Do nothing

                }

            case _ =>
                // Do nothing by default
        }

        // Check the children of this node
        for (child <- n.children)
            check (child.asInstanceOf[SourceNode])
    }

    /**
     * Are two types compatible?  If either of them are unknown then we
     * assume an error has already been raised elsewhere so we say they
     * are compatible with anything.  Otherwise the two types have to be
     * the same.
     */
    def iscompatible (t1 : Type, t2 : Type) : Boolean =
        (t1 == UnknownType ()) || (t2 == UnknownType ()) || (t1 == t2)

    /**
     * The entity defined by a defining occurrence of an identifier.
     * Defined by the context of the occurrence.
     */
    lazy val defentity : IdnDef => Entity =
        attr {
            case n =>
                n.parent match {
                    case decl : MainClass => MainClassEntity (decl)
                    case decl : Class     => ClassEntity (decl)
                    case decl : Field     => FieldEntity (decl)
                    case decl : Method    => MethodEntity (decl)
                    case decl : Argument  => ArgumentEntity (decl)
                    case decl : Var       => VariableEntity (decl)
                    case _                => UnknownEntity ()
                }
        }

    /**
     * The environment containing bindings for things that are being
     * defined. Much of the power of this definition comes from the Kiama
     * `chain` method, which threads the attribute through the tree in a
     * depth-first left-to-right order. The `envin` and `envout` definitions
     * are used to (optionally) update attribute value as it proceeds through
     * the tree.
     */
    lazy val defenv : Chain[SourceNode,Environment] =
        chain (defenvin, defenvout)

    def defenvin (in : SourceNode => Environment) : SourceNode ==> Environment = {

        // At the root, get a new empty environment
        case n : Program =>
            rootenv ()

        // At a nested scope region, create a new empty scope inside the outer
        // environment
        case n @ (_ : ClassBody | _ : MethodBody) =>
            enter (in (n))

    }

    def defenvout (out : SourceNode => Environment) : SourceNode ==> Environment = {

        // When leaving a nested scope region, remove the innermost scope from
        // the environment
        case n @ (_ : Class | _ : Method) =>
            leave (out (n))

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
     * The environment to use to lookup names at a node. Defined to be the
     * completed defining environment for the smallest enclosing scope.
     */
    lazy val env : SourceNode => Environment =
        attr {

            // At a scope-introducing node, get the final value of the
            // defining environment, so that all of the definitions of
            // that scope are present.
            case n @ (_ : Program | _ : Class | _ : Method) =>
                (n.lastChild[SourceNode])->defenv

            // Otherwise, ask our parent so we work out way up to the
            // nearest scope node ancestor (which represents the smallest
            // enclosing scope).
            case n =>
                (n.parent[SourceNode])->env

        }

    /**
     * The program entity referred to by an identifier definition or use.
     */
    lazy val entity : IdnNode => Entity =
        attr {

            // If we are looking at an identifier used as a method call,
            // we need to look it up in the environment of the class of
            // the object it is being called on. E.g., `o.m` needs to
            // look for `m` in the class of `o`, not in local environment.
            case HasParent (n @ IdnUse (i), CallExp (base, _, _)) =>
                (base->tipe) match {
                    case ReferenceType (decl) =>
                        findMethod (decl, i)
                    case t =>
                        UnknownEntity ()
                }

            // Otherwise, just look the identifier up in the environment
            // at the node. Return `UnknownEntity` if the identifier is
            // not defined.
            case n =>
                lookup (n->env, n.idn, UnknownEntity ())

        }

    /**
     * Find the entity for a method called `i` in the class defined by
     * `decl`. If the method is not found there and decl's class has a
     * superclass, look there. Repeat until either the definition is
     * found or no more superclasses exist, in which case return an
     * the unknown entity.
     */
    def findMethod (decl : Class, i : String) : Entity =
        lookup (decl->env, i, UnknownEntity ()) match {

            case UnknownEntity () =>
                // It's not in decl's env so see if there's a superclass.
                // If so, find the superclass decl and recursively look there.
                decl.superclass match {
                    case Some (superidn) =>
                        (superidn->entity) match {
                            case ClassEntity (superdecl) =>
                                // Superclass *is* a class
                                findMethod (superdecl, i)
                            case _ =>
                                // Superclass is something else
                                UnknownEntity ()
                        }
                    case None =>
                        UnknownEntity ()
                }

            case entity =>
                // Found it in decl's env, so return it
                entity

        }


    /**
     * Return the internal type of a syntactic type. In most cases they
     * are the same. The exception is class types since the class type
     * refers to the class by name, but we need to have it as a reference
     * type that refers to the declaration of that class.
     */
    def actualTypeOf (t : Type) : Type =
        t match {
            case ClassType (idn) =>
                (idn->entity) match {
                    case ClassEntity (decl) =>
                        ReferenceType (decl)
                    case _ =>
                        UnknownType ()
                }
            case _ =>
                t
        }

    /**
     * What is the type of an expression?
     */
    lazy val tipe : Expression => Type =
        attr {

            // Rule 4
            case _ : IntExp =>
                IntType ()

            // Rule 5
            case _ : TrueExp | _ : FalseExp =>
                BooleanType ()

            // Rule 6
            case IdnExp (i) =>
                (i->entity) match {
                    case ClassEntity (decl) =>
                        ReferenceType (decl)
                    case FieldEntity (decl) =>
                        actualTypeOf (decl.tipe)
                    case ArgumentEntity (decl) =>
                        actualTypeOf (decl.tipe)
                    case VariableEntity (decl) =>
                        actualTypeOf (decl.tipe)
                    case _ =>
                        UnknownType ()
                }

            // Rule 10
            case _ : IndExp =>
                IntType ()

            // Rule 11
            case _ : PlusExp | _ : MinusExp | _ : StarExp =>
                IntType ()

            // Rule 12
            case _ : AndExp =>
                BooleanType ()

            // Rule 13
            case _ : NotExp =>
                BooleanType ()

            // Rule 14
            case _ : LessExp =>
                BooleanType ()

            // Rule 15
            case _ : LengthExp =>
                IntType ()

            // Rule 16
            case CallExp (_, i, _) =>
                (i->entity) match {
                    case MethodEntity (decl) =>
                        actualTypeOf (decl.body.tipe)
                    case _ =>
                        UnknownType ()
                }

            // Rule 17
            case e : ThisExp =>
                e->thistype

            // Rule 18
            case _ : NewArrayExp =>
                IntArrayType ()

            // Rule 19:
            case NewExp (i) =>
                (i->entity) match {
                    case ClassEntity (decl) =>
                        ReferenceType (decl)
                    case _ =>
                        UnknownType ()
                }

            // Default: we know nothing
            case _ =>
                UnknownType ()

        }

    /**
     * The type of the normal class in which this node occurs.
     * Rule 17
     */
    lazy val thistype : SourceNode => Type =
        attr {

            // We got to the root without seeing a normal class, so
            // we don't know the type
            case n if n.isRoot =>
                UnknownType ()

            // We've reached a normal class node, so `this` is a
            // reference to that class
            case decl : Class =>
                ReferenceType (decl)

            // Otherwise, ask our parent
            case n =>
                (n.parent[SourceNode])->thistype

        }

    /**
     * Return the expected type of a method argument. We are given the declaration
     * of the method and the argument index. The latter is the count of the node
     * as a child of the CallExp node. The base expression is index 0, the name of
     * the called method is index 1, and the arguments start at index 2. Thus, to
     * lookup this argument in the formal arguments is we need to adjust by two.
     */
    def expTypeOfArg (method : Method, index : Int) : Type = {
        val argnum = index - 2
        val numargs = method.body.args.length
        if (argnum < numargs) {
            // Argument is in range, get the formal arg, return its type
            val arg = method.body.args (argnum)
            actualTypeOf (arg.tipe)
        } else
            // Not in range, so we don't know
            UnknownType ()
    }

    /**
     * What is the expected type of an expression?
     */
    lazy val exptipe : Expression => Type =
        attr {
            case e =>
                (e.parent) match {

                    // Rule 7
                    case _ : If | _ : While =>
                        BooleanType ()

                    // Rule 9
                    case VarAssign (lhs, _) =>
                        (lhs->entity) match {
                            case FieldEntity (Field (t, _)) =>
                                actualTypeOf (t)
                            case VariableEntity (Var (t, _)) =>
                                actualTypeOf (t)
                            case ArgumentEntity (Argument (t, _)) =>
                                actualTypeOf (t)
                            case _ =>
                                UnknownType ()
                        }

                    // Rule 10
                    case ArrayAssign (base, _, _) if base eq e =>
                        IntArrayType ()

                    // Rule 10
                    case ArrayAssign (_, index, _) if index eq e =>
                        IntType ()

                    // Rule 10
                    case ArrayAssign (_, _, elem) if elem eq e =>
                        IntType ()

                    // Rule 10
                    case IndExp (base, _) if base eq e =>
                        IntArrayType ()

                    // Rule 10
                    case IndExp (_, index) if index eq e =>
                        IntType ()

                    // Rule 11
                    case _ : PlusExp | _ : MinusExp | _ : StarExp =>
                        IntType ()

                    // Rule 12
                    case _ : AndExp =>
                        BooleanType ()

                    // Rule 13
                    case _ : NotExp =>
                        BooleanType ()

                    // Rule 14
                    case _ : LessExp =>
                        IntType ()

                    // Rule 15
                    case _ : LengthExp =>
                        IntArrayType ()

                    // Rule 16
                    case CallExp (base, u, _) if base eq e =>
                        UnknownType ()

                    case CallExp (_, u, _) =>
                        (u->entity) match {
                            case MethodEntity (decl) =>
                                expTypeOfArg (decl, e.index)

                            case _ =>
                                // No idea what is being called, so no type constraint
                                UnknownType ()
                        }

                    // Rule 18
                    case _ : NewArrayExp =>
                        IntType ()

                    // Rule 20
                    case MethodBody (t, _, _, _, _) =>
                        actualTypeOf (t)

                    // In all other cases, we don't care
                    case _ =>
                        UnknownType ()

                }
        }

}
