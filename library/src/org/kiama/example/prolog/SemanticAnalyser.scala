/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package example.prolog

import PrologTree.PrologTree
import org.kiama.util.Messaging

import org.kiama.attribution.Attribution

class SemanticAnalyser (tree : PrologTree) extends Attribution {

    import PrologTree._
    import SymbolTable._
    import org.kiama.util.Messaging.{check, collectmessages, message, Messages}
    import org.kiama.util.{Entity, UnknownEntity}
    import scala.collection.immutable.Seq

    /**
     * The semantic error messages for a given tree.
     */
    lazy val errors : Messages =
        collectmessages (tree) {
            case n @ Pred (s, ts) =>
                check (entity (n)) {
                    case Predicate (argtypes) if argtypes.length != ts.length =>
                        message (n, s"$s should have ${argtypes.length} arguments but has ${ts.length}")
                    case UnknownEntity () =>
                        message (n, s"$s is not declared")
                } ++
                checktype (n)

            case n @ Atom (s) =>
                check (entity (n)) {
                    case Predicate (argtypes) if argtypes.length != 0 =>
                        message (n, s"$s should have ${argtypes.length} arguments but has 0")
                    case UnknownEntity () =>
                        message (n, s"$s is not declared")
                } ++
                checktype (n)

            case n : Term =>
                checktype (n)
        }

    /**
     * Check types: issue a message if n's type is not compatible with its
     * expected type.  The unknown type is compatible with any other type.
     */
    def checktype (n : Term) : Messages =
        message (n, s"argument ${tipe (n)} found, ${exptipe (n)} expected",
                 (tipe (n) != UnknownType ()) && (exptipe (n) != UnknownType ()) &&
                    (tipe (n) != exptipe (n)))

    /**
     * Default environment.  Contains entities for the pre-defined
     * predicates for lists: cons and nil.
     */
    val defenv : Environment =
        rootenv ("nil" -> Predicate (Seq ()),
                 "cons" -> Predicate (Seq (UnknownType (), ListType ())))

    /**
     * The environment containing all bindings visible at a particular
     * node in the tree, not including any that are defined at that node.
     * If we are at the top of the tree, initialise the environment to
     * be empty.  Otherwise, if we are in a sequence, ask the previous
     * node for its environment, including any definitions there.  If
     * we are the first in a sequence or not in a sequence, ask the parent.
     */
    val envin : PrologNode => Environment =
        attr {
            case tree.prev (p) =>
                env (p)
            case tree.parent (p) =>
                envin (p)
            case _ =>
                defenv
        }

    /**
     * The environment containing all bindings visible "after" a
     * particular node in the tree.  The only nodes that add bindings
     * are the first occurrence of any particular predicate.  Other
     * nodes just pass the environment through: a node with children
     * gets the env coming out of its last child; a node with no
     * children just passes its own envin.
     *
     * For type checking we record the types of predicate arguments.
     * If this is the defining occurrence of a predicate, we obtain
     * the types of the actual arguments and use those.  Otherwise,
     * we use only those argument types for which we don't know
     * anything already.
     */
    val env : PrologNode => Environment =
        attr {
            case n @ Pred (s, ts) =>
                val argtypes = ts map tipe
                lookup (envin (n), s, UnknownEntity (), true) match {
                    case Predicate (oldargtypes) =>
                        val extargtypes = argtypes.padTo (oldargtypes.length, UnknownType ())
                        val newargtypes =
                            (oldargtypes, extargtypes).zipped.map {
                                case (UnknownType (), argtipe) =>
                                    argtipe
                                case (oldtipe, _) =>
                                    oldtipe
                            }
                        define (envin (n), s, Predicate (newargtypes))
                    case _ =>
                        define (envin (n), s, Predicate (argtypes))
                }
            case n @ Atom (s) if ! isDefinedInEnv (envin (n), s) =>
                define (envin (n), s, Predicate (Nil))
            case tree.lastChild (c) =>
                env (c)
            case n =>
                envin (n)
        }

    /**
     * The program entity referred to by a predicate or atom.  We just look in the
     * environment.  If it's not there, then use the unknown entity.  If we
     * have implemented the environments correctly, nothing can be unknown
     * since the first appearance is the defining ocurrence.
     */
    val entity : NamedLiteral => Entity =
        attr {
            case n @ Pred (s, ts) =>
                lookup (env (n), s, UnknownEntity (), true)
            case n @ Atom (s) =>
                lookup (env (n), s, UnknownEntity (), true)
        }

    /**
     * The entity for a given predicate or atom that is implied by the context.
     * This differs from entity because it doesn't account for information
     * implied by the node itself.  Used for type checking since we don't want
     * to use information from this node to check this node (a circularity).
     */
    val entityin : NamedLiteral => Entity =
        attr {
            case n @ Pred (s, ts) =>
                lookup (envin (n), s, UnknownEntity (), true)
            case n @ Atom (s) =>
                lookup (envin (n), s, UnknownEntity (), true)
        }

    /**
     * The environment of variables that are visible at this node.  This env
     * gets reset for each clause since the variables of one clause are not
     * related to those in the next clause.
     */
    val varsin : PrologNode => Environment =
        attr {
            case c : Clause =>
                rootenv ()
            case tree.prev (p) =>
                vars (p)
            case tree.parent (p) =>
                varsin (p)
        }

    /**
     * The environment containing visible variables *after* this node.  Only
     * updates at variable nodes.  If the variable has not been seen before,
     * we insert a binding to a new variable with a type give by the expected
     * type for the context.  Otherwise, if the variable has been seen before,
     * if it has an unknown type, then we update that type to the expected
     * type for the context.  Otherwise, the variable has been seen before and
     * already has a type constraint, so we don't change anything.
     */
    val vars : PrologNode => Environment =
        attr {
            case n @ Var (s) =>
                lookup (varsin (n), s, UnknownEntity (), true) match {
                    case Variable (UnknownType ()) =>
                        define (varsin (n), s, Variable (exptipe (n)))
                    case Variable (_) =>
                        varsin (n)
                    case UnknownEntity () =>
                        define (varsin (n), s, Variable (exptipe (n)))
                }
            case tree.lastChild (c) =>
                vars (c)
            case n =>
                varsin (n)
        }

    /**
     * The variable entity for a particular variable name, given by
     * the context before this occurrence.
     */
    val varentity : Var => Entity =
        attr {
            case n @ Var (s) =>
                lookup (varsin (n), s, UnknownEntity (), true)
        }

    /**
     * The type of a term given by the previous uses.
     */
    val tipe : Term => Type =
        attr {
            case Atom (_)         => AtomType ()
            case Integer (_)      => IntegerType ()
            case Pred ("cons", _) => ListType ()
            case Pred ("nil", _)  => ListType ()
            case n @ Var (s) =>
                varentity (n) match {
                    case Variable (t) => t
                    case _            => UnknownType ()
                }
            case _                => UnknownType ()
        }

    /**
     * The expected type of a term, given by the context.  The only
     * place that terms can be used is as predicate arguments, so we
     * look up the predicate to find out what its argument constraints
     * are and select the one that corresponds to this argument.  The
     * index property gives us the position of the variable in the
     * argument list.
     */
    val exptipe : Term => Type =
        attr {
            case tree.parent.pair (n, p : Pred) =>
                entityin (p) match {
                    case Predicate (argtypes) =>
                        val i = tree.index (n)
                        if (i < argtypes.length)
                            argtypes (i)
                        else
                            UnknownType ()
                    case _ =>
                        UnknownType ()
                }
            case _ =>
                UnknownType ()
        }

}
