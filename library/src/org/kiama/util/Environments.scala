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
import scala.collection.immutable.Stack

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
     * Reset the environment module.
     */
    def resetEnvironments {
        nameCounter.reset ()
    }

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
    type Environment = Stack[Scope]

    /**
     * Create a root environment, i.e., one that has a single scope containing
     * the given bindings.
     */
    def rootenv (bindings : (String,Entity)*) : Environment =
        Stack (immutable.HashMap (bindings : _*))

    /**
     * Enter a new empty scope nested within the given environment.
     */
    def enter (env : Environment) : Environment =
        env.push (new immutable.HashMap[String,Entity])

    /**
     * Leave the outermost scope of the given environment, raising an error if
     * the environment is empty.
     */
    def leave (env : Environment) : Environment =
        if (env.isEmpty)
            sys.error ("leave called on empty environment")
        else
            env.pop

    /**
     * Define i to be e in the current scope of env, raising an error if the
     * environment is empty.
     */
    def define (env : Environment, i : String, e : Entity) : Environment = {
        if (env.isEmpty)
            sys.error ("define called on empty environment")
        else {
            val s = env.top
            env.pop.push (s + ((i, e)))
        }
    }

    /**
     * Say whether i is defined in the current scope of env.
     */
    def isDefinedInScope (env : Environment, i : String) : Boolean =
        env.nonEmpty && ((env.top) contains i)

    /**
     * Say whether i is defined in the given scope.
     */
    def isDefinedInScope (scope : Scope, i : String) : Boolean =
        scope contains i

    /**
     * Say whether i is defined in any scope of env.
     */
    def isDefinedInEnv (env : Environment, i : String) : Boolean =
        env.exists (s => isDefinedInScope (s, i))

    /**
     * Say whether i is defined in an innermost scope of env (i.e., in the
     * current scope).
     */
    def isDefinedInInner (env : Environment, i : String) : Boolean =
        isDefinedInScope (env.top, i)

    /**
     * Say whether i is defined in an outer scope of env (i.e., not in the
     * current scope).
     */
    def isDefinedInOuter (env : Environment, i : String) : Boolean =
        isDefinedInEnv (env.pop, i)

    /**
     * Look up i in env, returning the mapped Entity if there is one, otherwise
     * return e.  If scope is true, just search the innermost scope, otherwise
     * search outwards in all scopes, returning the first Entity found, if any.
     */
    def lookup (env : Environment, i : String, e : Entity, scope : Boolean = false) : Entity =
        if (env.isEmpty)
            e
        else if (scope)
            env.top.getOrElse (i, e)
        else if (isDefinedInScope (env, i))
            env.top (i)
        else
            lookup (env.pop, i, e)

}
