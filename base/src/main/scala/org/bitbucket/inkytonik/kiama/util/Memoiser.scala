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
 * The types of memoiser that can be created.
 */
abstract class MemoiserType

/**
 * Value-based key comparison.
 */
case object ValueKeys extends MemoiserType

/**
 * Identity-based key comparison.
 */
case object IdentityKeys extends MemoiserType

/**
 * A memoiser that can store arbitrary values of type `U` under keys of
 * type `T`. The behaviour of the memoiser can be adjusted by selecting
 * an appropriate type.
 */
class Memoiser[T, U](tipe : MemoiserType) {

    import com.google.common.collect.MapMaker
    import java.util.concurrent.ConcurrentMap
    import scala.collection.JavaConverters._

    /**
     * The cache for this instance.
     */
    private[this] val map : ConcurrentMap[T, U] =
        (tipe match {
            case ValueKeys =>
                new MapMaker()
            case IdentityKeys =>
                new MapMaker().weakKeys
        }).concurrencyLevel(1).makeMap()

    /**
     * Get the value stored at key `t` or return null if no value.
     */
    def apply(t : T) : U =
        map.get(t)

    /**
     * Duplicate an entry if possible. If `t1` has a memoised value associated
     * with it, set the value associated with `t2` to the same value. If there
     * is no value associated with `t1`, do nothing.
     */
    def dup(t1 : T, t2 : T) {
        val u = map.get(t1)
        if (u != null)
            put(t2, u)
    }

    /**
     * Return the value stored at key `t` as an option.
     */
    def get(t : T) : Option[U] =
        Option(map.get(t))

    /**
     * Return the value stored at key `t` if there is one, otherwise
     * return `u`. `u` is only evaluated if necessary.
     */
    def getOrDefault(t : T, u : => U) : U = {
        val v = map.get(t)
        if (v == null)
            u
        else
            v
    }

    /**
     * Has the value at `t` already been computed or not? By default, does
     * the memo table contain a value for `t`?
     */
    def hasBeenComputedAt(t : T) : Boolean =
        map.get(t) != null

    /**
     * Get the value stored at key `t` or return null if no value.
     */
    def image(t : T) : U =
        map.get(t)

    /**
     * A view of the set of keys that are currently in this memo table.
     */
    def keys : Vector[T] =
        map.keySet.asScala.toVector

    /**
     * Store the value `u` under the key `t`.
     */
    def put(t : T, u : U) {
        map.put(t, u)
    }

    /**
     * Store the value `u` under the key `t` if `t` does not already have an
     * associated value.
     */
    def putIfAbsent(t : T, u : U) {
        map.putIfAbsent(t, u)
    }

    /**
     * Immediately reset the memo table.
     */
    def reset() {
        map.clear()
    }

    /**
     * Immediately reset the memo table at `t`.
     */
    def resetAt(t : T) {
        map.remove(t)
    }

    /**
     * The number of entries in the memo table.
     */
    def size() : Long =
        map.size

    /**
     * Update the value associated with `t` by applying `f` to it. If there
     * is no value currently associated with `t`, associate it with `u`. `u`
     * is only evaluated if necessary.
     */
    def updateAt(t : T, f : U => U, u : => U) {
        val v = map.get(t)
        if (v == null)
            put(t, u)
        else
            put(t, f(v))
    }

    /**
     * A view of the set of values that are currently in this memo table.
     */
    def values : Vector[U] =
        map.values.asScala.toVector

}

/**
 * Support for memoisers.
 */
object Memoiser {

    /**
     * Make a new memoiser that strongly holds onto its keys and uses object
     * value to compare them.
     */
    def makeMemoiser[T, U]() : Memoiser[T, U] =
        new Memoiser(ValueKeys)

    /**
     * Make a new memoiser that weakly holds onto its keys and uses object
     * identity to compare them.
     */
    def makeIdMemoiser[T, U]() : Memoiser[T, U] =
        new Memoiser(IdentityKeys)

}
