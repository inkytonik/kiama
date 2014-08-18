/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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
package util

import scala.collection.immutable

/**
 * General implementation of environments as stacked scopes.  The objects
 * associated with names in environments are of type Entity.
 */
trait Environments {

    /**
     * A counter to count generated names.
     */
    val nameCounter = new Counter (0)

    /**
     * Support for unique ids for named things.
     */
    trait Named {

        /**
         * The underlying name of this thing.
         */
        def ident : String

        /**
         * A unique number to represent this thing.
         */
        val num = nameCounter.next ()

        /**
         * A unique identifier for this thing, incorporating the underlying
         * name and its unique number.
         */
        val id = ident + num

        /**
         * Still print named things as their underlying name.
         */
        override def toString : String = ident

    }

    /**
     * A named entity.
     */
    trait NamedEntity extends Entity with Named

    /**
     * A scope maps identifiers to entities.
     */
    type Scope = Map[String,Entity]

    /**
     * An environment is a stack of scopes with the innermost scope on the top.
     */
    type Environment = List[Scope]

    /**
     * Create a root environment, i.e., one that has a single scope containing
     * the given bindings.
     */
    def rootenv (bindings : (String,Entity)*) : Environment =
        List (immutable.HashMap (bindings : _*))

    /**
     * Enter a new empty scope nested within the given environment.
     */
    def enter (env : Environment) : Environment =
        (new immutable.HashMap[String,Entity]) :: env

    /**
     * Leave the outermost scope of the given environment, raising an error if
     * the environment is empty.
     */
    def leave (env : Environment) : Environment =
        env match {
            case _ :: t => t
            case _      => sys.error ("leave called on empty environment")
        }

    /**
     * Define `i` to be `e` in the current scope of `env`, raising an error if
     * the environment is empty.
     */
    def define (env : Environment, i : String, e : Entity) : Environment =
        env match {
            case s :: t => (s + ((i, e))) :: t
            case _      => sys.error ("define called on empty environment")
        }

    /**
     * Say whether `i` is defined in the current scope of `env`.
     */
    def isDefinedInScope (env : Environment, i : String) : Boolean =
        env match {
            case s :: _ => s contains i
            case _      => false
        }

    /**
     * Say whether `i` is defined in the given scope.
     */
    def isDefinedInScope (scope : Scope, i : String) : Boolean =
        scope contains i

    /**
     * Say whether `i` is defined in any scope of `env`.
     */
    def isDefinedInEnv (env : Environment, i : String) : Boolean =
        env.exists (s => isDefinedInScope (s, i))

    /**
     * Say whether `i` is defined in an innermost scope of `env` (i.e., in the
     * current scope).
     */
    def isDefinedInInner (env : Environment, i : String) : Boolean =
        env match {
            case s :: _ => isDefinedInScope (s, i)
            case _      => false
        }

    /**
     * Say whether `i` is defined in an outer scope of `env` (i.e., not in the
     * current scope).
     */
    def isDefinedInOuter (env : Environment, i : String) : Boolean =
        env match {
            case _ :: t => isDefinedInEnv (t, i)
            case _      => false
        }

    /**
     * Look up `i` in `env`, returning the mapped Entity if there is one,
     * otherwise return `e`.  If `local` is true, just search the innermost
     * scope, otherwise search outwards in all scopes, returning the first
     * entity found, if any.
     */
    def lookup (env : Environment, i : String, e : => Entity, local : Boolean = false) : Entity =
        env match {
            case s :: t =>
                if (local)
                    s.getOrElse (i, e)
                else if (isDefinedInScope (env, i))
                    s (i)
                else
                    lookup (t, i, e)
            case _ =>
                e
        }

}
