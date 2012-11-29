/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2012 Anthony M Sloane, Macquarie University.
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

import scala.collection.immutable.{HashMap, Stack}

/**
 * General implementation of environments as stacked scopes.  The objects
 * associated with names in environments are of type Entity.
 */
trait Environments {

    /**
     * An entity that represents some program object.
     */
    abstract class Entity

    /**
     * Reset the environment module.
     */
    def resetEnvironments {
        Named.resetCount
    }

    /**
     * Static support for entity names.
     */
    object Named {

        /**
         * Count of the named things that have been produced so far.
         */
        var count = 0

        /**
         * Reset the entity count.
         */
        def resetCount {
            count = 0
        }

    }

    /**
     * Support for unique ids for named things.
     */
    trait Named {

        import Named.count

        /**
         * The underlying name of this thing.
         */
        def ident : String

        /**
         * A unique number to represent this thing.
         */
        private val num = {
            count = count + 1
            count
        }

        /**
         * A unique identifier for this thing, incorporating the underlying
         * name and its unique number.
         */
        val id = ident + num

        /**
         * Still print named things as their underlying name.
         */
        override def toString = ident

    }

    /**
     * A named entity.
     */
    trait NamedEntity extends Entity with Named

    /**
     * An entity that represents an error situation.  These entities are
     * usually accepted in most situations to avoid cascade errors.
     */
    trait ErrorEntity extends Entity
    
    /**
     * A entity represented by names for whom we have seen more than one
     * declaration so we are unsure what is being represented.
     */
    case class MultipleEntity () extends ErrorEntity {
        lazy val ident = "multiple"
    }

    /**
     * An unknown entity, represented by names whose declarations are missing.
     */
    case class UnknownEntity () extends ErrorEntity {
        lazy val ident = "unknown"
    }

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
        Stack (HashMap (bindings : _*))
    
    /**
     * Enter a new empty scope nested within the given environment.
     */
    def enter (env : Environment) : Environment =
        env.push (new HashMap[String,Entity])

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
    def lookup (env : Environment, i : String, e : Entity, scope : Boolean = false) : Entity = {

        def lookupscope (s : Scope) : Entity =
            s.getOrElse (i, e)

        if (env.isEmpty)
            e
        else if (scope)
            lookupscope (env.top)
        else {
            for (s <- env)
                if (s contains i)
                    return s (i)
            e
        }
        
    }    

}
