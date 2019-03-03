/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package util

/**
 * General implementation of environments as stacked scopes.  The objects
 * associated with names in environments are of type `E` which must be a
 * subtype of the general Entity class.
 */
trait Environments[E <: Entity] {

    import scala.collection.immutable.HashMap

    /**
     * A counter to count generated names.
     */
    val nameCounter = new Counter(0)

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
        val num = nameCounter.next()

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
     * A scope maps identifiers to entities.
     */
    type Scope = Map[String, E]

    /**
     * An environment is a stack of scopes with the innermost scope on the top.
     */
    type Environment = List[Scope]

    /**
     * Create a root environment, i.e., one that has a single scope containing
     * the given bindings.
     */
    def rootenv(bindings : (String, E)*) : Environment =
        List(HashMap(bindings : _*))

    /**
     * Enter a new empty scope nested within the given environment.
     */
    def enter(env : Environment) : Environment =
        (new HashMap[String, E]) :: env

    /**
     * Leave the outermost scope of the given environment, raising an error if
     * the environment is empty.
     */
    def leave(env : Environment) : Environment =
        env match {
            case _ :: t => t
            case _      => sys.error("leave called on empty environment")
        }

    /**
     * Define `i` to be `e` in the current scope of `env`, raising an error if
     * the environment is empty.
     */
    def define(env : Environment, i : String, e : E) : Environment =
        env match {
            case s :: t => (s + ((i, e))) :: t
            case _      => sys.error("define called on empty environment")
        }

    /**
     * As for `define`, except if `i` is already defined in the innermost
     * scope of `env`, define it to be `MultipleEntity` instead. The entity
     * `e` is only evaluated if needed.
     */
    def defineIfNew(env : Environment, i : String, eold : E, enew : => E) : Environment =
        define(env, i, if (isDefinedInScope(env, i)) eold else enew)

    /**
     * Say whether `i` is defined in the current scope of `env`.
     */
    def isDefinedInScope(env : Environment, i : String) : Boolean =
        env match {
            case s :: _ => s contains i
            case _      => false
        }

    /**
     * Say whether `i` is defined in the given scope.
     */
    def isDefinedInScope(scope : Scope, i : String) : Boolean =
        scope contains i

    /**
     * Say whether `i` is defined in any scope of `env`.
     */
    def isDefinedInEnv(env : Environment, i : String) : Boolean =
        env.exists(s => isDefinedInScope(s, i))

    /**
     * Say whether `i` is defined in an innermost scope of `env` (i.e., in the
     * current scope).
     */
    def isDefinedInInner(env : Environment, i : String) : Boolean =
        env match {
            case s :: _ => isDefinedInScope(s, i)
            case _      => false
        }

    /**
     * Say whether `i` is defined in an outer scope of `env` (i.e., not in the
     * current scope).
     */
    def isDefinedInOuter(env : Environment, i : String) : Boolean =
        env match {
            case _ :: t => isDefinedInEnv(t, i)
            case _      => false
        }

    /**
     * Look up `i` in `env`, returning the mapped Entity if there is one,
     * otherwise return `e`.  If `local` is true, just search the innermost
     * scope, otherwise search outwards in all scopes, returning the first
     * entity found, if any.
     */
    def lookup(env : Environment, i : String, e : => E, local : Boolean = false) : E =
        env match {
            case s :: t =>
                if (local)
                    s.getOrElse(i, e)
                else if (isDefinedInScope(env, i))
                    s(i)
                else
                    lookup(t, i, e)
            case _ =>
                e
        }

    /**
     * Pretty-print the environment `env`.
     */
    def format(env : Environment) : String = {

        import org.bitbucket.inkytonik.kiama.output.PrettyPrinter._

        def entryToDoc(entry : (String, Entity)) : Doc =
            dquotes(entry._1) <+> "->" <+> value(entry._2)

        def scopeToDoc(s : Scope) : Doc =
            "scope" <> nest(line <> vsep((s map entryToDoc).toVector))

        env match {
            case Nil =>
                "no scopes"
            case ss =>
                layout(vsep(ss map scopeToDoc))
        }

    }

}
