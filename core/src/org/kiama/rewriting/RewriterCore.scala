/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
package rewriting

/**
 * Core implementation of strategy-based rewriting. Implement and construct
 * basic strategies and rewrite rules.
 */
trait RewriterCore {

    import org.kiama.util.Emitter
    import scala.collection.generic.CanBuildFrom
    import scala.collection.mutable.Builder
    import scala.collection.mutable.WeakHashMap
    import scala.language.higherKinds
    import scala.language.experimental.macros

    /**
     * Construct a strategy that always succeeds, changing the subject term to
     * the given term `t`. The term `t` is evaluated at most once.
     */
    def build (t : Any) : Strategy =
        macro RewriterMacros.buildMacro

    /**
     * As for the other `build` with the first argument specifying a name for
     * the constructed strategy.
     */
    def build (n : String, t : => Any) : Strategy =
        rulef (n, _ => t)

    /**
     * A strategy that always succeeds with the subject term unchanged (i.e.,
     * this is the identity strategy) with the side-effect that the subject
     * term is printed to the given emitter, prefixed by the string `s`.  The
     * emitter defaults to one that writes to standard output.
     */
    def debug (msg : String, emitter : Emitter = new Emitter) : Strategy =
        macro RewriterMacros.debugMacro

    /**
     * As for the other `debug` with the first argument specifying a name for
     * the constructed strategy.
     */
    def debug (n : String, msg : String, emitter : Emitter) : Strategy =
        strategyf (n, t => { emitter.emitln (msg + t); Some (t) })

    /**
     * Create a logging strategy based on a strategy `s`. The returned strategy
     * succeeds or fails exactly as `s` does, but also prints the provided message,
     * the subject term, the success or failure status, and on success, the result
     * term, to the provided emitter (default: standard output). `s` is evaluated
     * at most once.
     */
    def log (s : Strategy, msg : String, emitter : Emitter = new Emitter) : Strategy =
        macro RewriterMacros.logMacro

    /**
     * As for the other `log` with the first argument specifying a name for
     * the constructed strategy.
     */
    def log (n : String, s : => Strategy, msg : String, emitter : Emitter) : Strategy =
        new Strategy {
            val name = n
            def apply (t1 : Any) : Option[Any] = {
                emitter.emit (msg + t1)
                val r = s (t1)
                r match {
                    case Some (t2) =>
                        emitter.emitln (" succeeded with " + t2)
                    case None =>
                        emitter.emitln (" failed")
                }
                r
            }
        }

    /**
     * Create a logging strategy based on a strategy `s`.  The returned strategy
     * succeeds or fails exactly as `s` does, but if `s` fails, also prints the
     * provided message and the subject term to the provided emitter (default:
     * standard output). `s` is evaluated at most once.
     */
    def logfail[T] (s : Strategy, msg : String, emitter : Emitter = new Emitter) : Strategy =
        macro RewriterMacros.logfailMacro

    /**
     * As for the other `logfail` with the first argument specifying a name for
     * the constructed strategy.
     */
    def logfail[T] (n : String, s : => Strategy, msg : String, emitter : Emitter) : Strategy =
        new Strategy {
            val name = n
            def apply (t1 : Any) : Option[Any] = {
                val r = s (t1)
                r match {
                    case Some (t2) =>
                        // Do nothing
                    case None =>
                        emitter.emitln (msg + t1 + " failed")
                }
                r
            }
        }

    /**
     * Return a strategy that behaves as `s` does, but memoises its arguments and
     * results.  In other words, if `memo(s)` is called on a term `t` twice, the
     * second time will return the same result as the first, without having to
     * invoke `s`.  For best results, it is important that `s` should have no side
     * effects. `s` is evaluated at most once.
     */
    def memo (s : Strategy) : Strategy =
        macro RewriterMacros.memoMacro

    /**
     * As for the other `memo` with the first argument specifying a name for
     * the constructed strategy.
     */
    def memo (n : String, s : => Strategy) : Strategy =
        new Strategy {
            val name = n
            private val cache =
                new scala.collection.mutable.HashMap[Any,Option[Any]]
            private lazy val strat = s
            def apply (t : Any) : Option[Any] =
                cache.getOrElseUpdate (t, strat (t))
        }

    /**
     * Construct a strategy from an option value `o`. The strategy succeeds
     * or fails depending on whether `o` is a Some or None, respectively.
     * If `o` is a `Some`, then the subject term is changed to the term that
     * is wrapped by the `Some`. `o` is evaluated at most once.
     */
    def option (o : Option[Any]) : Strategy =
        macro RewriterMacros.optionMacro

    /**
     * As for the other `option` with the first argument specifying a name for
     * the constructed strategy.
     */
    def option (n : String, o : => Option[Any]) : Strategy =
        strategyf (n, _ => o)

    /**
     * Perform a paramorphism over a value. This is a fold in which the
     * recursive step may refer to the recursive component of the value
     * and the results of folding over the children.  When the function `f`
     * is called, the first parameter is the value and the second is a
     * sequence of the values that `f` has returned for the children.  his
     * will work on any value, but will only decompose values that are
     * supported by the `Term` generic term deconstruction.  This operation
     * is similar to that used in the Uniplate library.
     */
    def para[T] (f : (Any, Seq[T]) => T) : Any => T = {
        case Term (t, ts) => f (t, ts.map (para (f)))
    }

    /**
     * Define a term query by a partial function `f`. The query always succeeds
     * with no effect on the subject term but applies the given partial function
     * `f` to the subject term.  In other words, the strategy runs `f` for its
     * side-effects.
     */
    def query[T] (f : Any ==> T) : Strategy =
        macro RewriterMacros.queryMacro[T]

    /**
     * As for the other `query` with the first argument specifying a name for
     * the constructed strategy.
     */
    def query[T] (n : String, f : Any ==> T) : Strategy =
        new Strategy {
            val name = n
            def apply (t : Any) : Option[Any] = {
                if (f isDefinedAt t)
                    f (t)
                Some (t)
            }
        }

    /**
     * Define a term query by a function `f`. The query always succeeds with
     * no effect on the subject term but applies the given (possibly partial)
     * function `f` to the subject term.  In other words, the strategy runs
     * `f` for its side-effects.
     */
    def queryf[T] (f : Any => T) : Strategy =
        macro RewriterMacros.queryfMacro[T]

    /**
     * As for the other `queryf` with the first argument specifying a name for
     * the constructed strategy.
     */
    def queryf[T] (n : String, f : Any => T) : Strategy =
        new Strategy {
            val name = n
            def apply (t : Any) : Option[Any] = {
                f (t)
                Some (t)
            }
        }

    /**
     * Define a rewrite rule using a partial function `f`. If the function is
     * defined at the current term, then the strategy succeeds with the return
     * value of the function applied to the current term. Otherwise the
     * strategy fails.
     */
    def rule (f : Any ==> Any) : Strategy =
        macro RewriterMacros.ruleMacro

    /**
     * As for the other `rule` with the first argument specifying a name for
     * the constructed strategy.
     */
    def rule (n : String, f : Any ==> Any) : Strategy =
        new Strategy {
            val name = n
            def apply (t : Any) : Option[Any] = {
                if (f isDefinedAt t)
                    Some (f (t))
                else
                    None
            }
        }

    /**
     * Define a rewrite rule using a function `f` that returns a term.
     * The rule always succeeds with the return value of the function.
     */
    def rulef (f : Any => Any) : Strategy =
        macro RewriterMacros.rulefMacro

    /**
     * As for the other `rulef` with the first argument specifying a name for
     * the constructed strategy.
     */
    def rulef (n : String, f : Any => Any) : Strategy =
        strategyf (n, t => Some (f (t)))

    /**
     * Define a rewrite rule using a function `f` that returns a strategy.  The
     * rule applies the function to the subject term to get a strategy which
     * is then applied again to the subject term. In other words, the function
     * is only used for effects such as pattern matching.  The whole thing also
     * fails if `f` is not defined at the term in the first place.
     */
    def rulefs (f : Any ==> Strategy) : Strategy =
        macro RewriterMacros.rulefsMacro

    /**
     * As for the other `rulefs` with the first argument specifying a name for
     * the constructed strategy.
     */
    def rulefs (n : String, f : Any ==> Strategy) : Strategy =
        new Strategy {
            val name = n
            def apply (t : Any) : Option[Any] = {
                if (f isDefinedAt t)
                    (f (t)) (t)
                else
                    None
            }
        }

    /**
     * Make a strategy from a partial function `f`. If the function is
     * defined at the current term, then the function return value
     * when applied to the current term determines whether the strategy
     * succeeds or fails. If the function is not defined at the current
     * term, the strategy fails.
     */
    def strategy (f : Any ==> Option[Any]) : Strategy =
        macro RewriterMacros.strategyMacro

    /**
     * As for the other `strategy` with the first argument specifying a name for
     * the constructed strategy.
     */
    def strategy (n : String, f : Any ==> Option[Any]) : Strategy =
        new Strategy {
            val name = n
            def apply (t : Any) : Option[Any] = {
                if (f isDefinedAt t)
                    f (t)
                else
                    None
            }
        }

    /**
     * Make a strategy from a function `f`. The function return value
     * determines whether the strategy succeeds or fails.
     */
    def strategyf (f : Any => Option[Any]) : Strategy =
        macro RewriterMacros.strategyfMacro

    /**
     * As for the other `strategyf` with the first argument specifying a name for
     * the constructed strategy.
     */
    def strategyf (n : String, f : Any => Option[Any]) : Strategy =
        new Strategy {
            val name = n
            def apply (t : Any) : Option[Any] =
                f (t)
        }

    /**
     * Construct a strategy that succeeds only if the subject term matches
     * the given term `t`.
     */
    def term (t : Any) : Strategy =
        macro RewriterMacros.termMacro

    /**
     * As for the other `term` with the first argument specifying a name for
     * the constructed strategy.
     */
    def term (n : String, t : Any) : Strategy =
        rule (n, {
            case `t` => t
        })

    // Product duplication support

    /**
     * Cache of constructors for product duplication.
     */
    protected val constrcache =
        new WeakHashMap[java.lang.Class[_], java.lang.reflect.Constructor[_]]

    /**
     * General product duplication function.  Returns a product that applies
     * the same constructor as the product `t`, but with the given children
     * instead of `t`'s children.  Fails if a constructor cannot be found,
     * there are the wrong number of new children, or if one of the new
     * children is not of the appropriate type.
     */
    protected def dup[T <: Product] (t : T, children : Array[AnyRef]) : T = {
        val clazz = t.getClass
        val ctor = constrcache.getOrElseUpdate (clazz, (clazz.getConstructors())(0))
        try {
            ctor.newInstance (children : _*).asInstanceOf[T]
        } catch {
            case e : IllegalArgumentException =>
                sys.error ("dup illegal arguments: " + ctor + " (" +
                           children.deep.mkString (",") + "), expects " +
                           ctor.getParameterTypes.length)
        }
    }

    /**
     * Make an arbitrary value `c` into a term child, checking that it worked
     * properly. Object references will be returned unchanged; other values
     * will be boxed.
     */
    protected def makechild (c : Any) : AnyRef =
        c.asInstanceOf[AnyRef]

    // Generic traversals

    /**
     * Traversal to a single child.  Construct a strategy that applies `s` to
     * the ''ith'' child of the subject term (counting from one).  If `s` succeeds on
     * the ''ith'' child producing `t`, then succeed, forming a new term that is the
     * same as the original term except that the ''ith'' child is now `t`.  If `s` fails
     * on the ''ith'' child or the subject term does not have an ''ith'' child, then fail.
     * `child(i, s)` is equivalent to Stratego's `i(s)` operator.  If `s` succeeds on
     * the ''ith'' child producing the same term (by `eq` for references and by `==` for
     * other values), then the overall strategy returns the subject term.
     * This operation works for instances of `Product` or finite `Seq` values.
     * `s` is evaluated at most once.
     */
    def child (i : Int, s : Strategy) : Strategy =
        macro RewriterMacros.childMacro

    /**
     * As for the other `child` with the first argument specifying a name for
     * the constructed strategy.
     */
    def child (n : String, i : Int, s : => Strategy) : Strategy =
        new Strategy {

            val name = n

            lazy val strat = s

            def apply (t : Any) : Option[Any] =
                t match {
                    case p : Product => childProduct (strat, i, p)
                    case t : Seq[_]  => childSeq (strat, i, t.asInstanceOf[Seq[Any]])
                    case _           => None
                }

        }

    def childProduct (s : Strategy, i : Int, p : Product) : Option[Any] = {
        val numchildren = p.productArity
        if ((i < 1) || (i > numchildren)) {
            None
        } else {
            val ct = p.productElement (i-1)
            s (ct) match {
                case Some (ti) if (same (ct, ti)) =>
                    Some (p)
                case Some (ti) =>
                    val newchildren = new Array[AnyRef](numchildren)
                    var j = 0
                    while (j < numchildren) {
                        newchildren (j) = makechild (p.productElement (j))
                        j = j + 1
                    }
                    newchildren (i-1) = makechild (ti)
                    val ret = dup (p, newchildren)
                    Some (ret)
                case None =>
                    None
            }
        }
    }

    def childSeq[CC[U] <: Seq[U]] (s : Strategy, i : Int, t : CC[Any])
            (implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]])
                        : Option[CC[Any]] = {
        val numchildren = t.size
        if ((i < 1) || (i > numchildren)) {
            None
        } else {
            val ct = t (i - 1)
            s (ct) match {
                case Some (ti) if (same (ct, ti)) =>
                    Some (t)
                case Some (ti) =>
                    val b = cbf (t)
                    b.sizeHint (t.size)
                    var j = 0
                    while (j < i - 1) {
                        b += t (j)
                        j = j + 1
                    }
                    b += ti
                    j = j + 1
                    while (j < numchildren) {
                        b += t (j)
                        j = j + 1
                    }
                    Some (b.result)
                case None =>
                    None
            }
        }
    }

    /**
     * Compare two arbitrary values. If they are both references, use
     * reference equality, otherwise throw an error since we should be
     * able to cast anything to reference.
     */
    protected def same (v1 : Any, v2 : Any) : Boolean =
        if (v1 == null)
            v2 == null
        else if (v2 == null)
            false
        else
            (v1, v2) match {
                case (r1 : AnyRef, r2: AnyRef) =>
                    r1 eq r2
                case _ =>
                    sys.error ("Rewriter.same: comparison of non-AnyRefs " + v1 + " and " +
                               v2 + ", should not be reached")
            }

    /**
     * Traversal to all children.  Construct a strategy that applies `s` to all
     * term children of the subject term.  If `s` succeeds on all of the children,
     * then succeed, forming a new term from the constructor
     * of the original term and the result of `s` for each child.  If `s` fails on any
     * child, fail. If there are no children, succeed.  If `s` succeeds on all
     * children producing the same terms (by `eq` for references and by `==` for
     * other values), then the overall strategy returns the subject term.
     * This operation works on finite `Rewritable`, `Product`, `Map` and `Traversable`
     * values, checked for in that order.
     * Children of a `Rewritable` (resp. Product, collection) value are processed
     * in the order returned by the value's deconstruct (resp. `productElement`,
     * `foreach`) method.
     * `s` is evaluated at most once.
     */
    def all (s : Strategy) : Strategy =
        macro RewriterMacros.allMacro

    /**
     * As for the other `all` with the first argument specifying a name for
     * the constructed strategy.
     */
    def all (n : String, s : => Strategy) : Strategy =
        new Strategy {

            val name = n

            lazy val strat = s

            def apply (t : Any) : Option[Any] =
                t match {
                    case r : Rewritable     => allRewritable (strat, r)
                    case p : Product        => allProduct (strat, p)
                    case m : Map[_,_]       => allMap (strat, m.asInstanceOf[Map[Any,Any]])
                    case t : Traversable[_] => allTraversable (strat, t.asInstanceOf[Traversable[Any]])
                    case _                  => Some (t)
                }

        }

    def allProduct (s : Strategy, p : Product) : Option[Any] = {
        val numchildren = p.productArity
        if (numchildren == 0)
            Some (p)
        else {
            val newchildren = new Array[AnyRef](numchildren)
            var changed = false
            var i = 0
            while (i < numchildren) {
                val ct = p.productElement (i)
                s (ct) match {
                    case Some (ti) =>
                        newchildren (i) = makechild (ti)
                        if (!same (ct, ti))
                            changed = true
                    case None =>
                        return None
                }
                i = i + 1
            }
            if (changed) {
                val ret = dup (p, newchildren)
                Some (ret)
            } else
                Some (p)
        }
    }

    def allRewritable (s : Strategy, r : Rewritable) : Option[Any] = {
        val numchildren = r.arity
        if (numchildren == 0)
            Some (r)
        else {
            val children = r.deconstruct
            val newchildren = new Array[Any](numchildren)
            var changed = false
            var i = 0
            while (i < numchildren) {
                val ct = children (i)
                s (ct) match {
                    case Some (ti) =>
                        newchildren (i) = makechild (ti)
                        if (!same (ct, ti))
                            changed = true
                    case None =>
                        return None
                }
                i = i + 1
            }
            if (changed) {
                val ret = r.reconstruct (newchildren)
                Some (ret)
            } else
                Some (r)
        }
    }

    def allTraversable[CC[_] <: Traversable[Any]] (s : Strategy, t : CC[Any])
            (implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]])
                        : Option[CC[Any]] =
        if (t.size == 0)
            Some (t)
        else {
            val b = cbf (t)
            b.sizeHint (t.size)
            var changed = false
            for (ct <- t)
                s (ct) match {
                    case Some (ti) =>
                        b += ti
                        if (!same (ct, ti))
                            changed = true
                    case None =>
                        return None
                }
            if (changed)
                Some (b.result)
            else
                Some (t)
        }

    def allMap[CC[V,W] <: Map[V,W]] (s : Strategy, t : CC[Any,Any])
            (implicit cbf : CanBuildFrom[CC[Any,Any], (Any, Any), CC[Any,Any]])
                        : Option[CC[Any,Any]] =
        if (t.size == 0)
            Some (t)
        else {
            val b = cbf (t)
            b.sizeHint (t.size)
            var changed = false
            for (ct <- t)
                s (ct) match {
                    case Some (ti @ (tix,tiy)) =>
                        b += ti
                        if (!same (ct, ti))
                            changed = true
                    case _ =>
                        return None
                }
            if (changed)
                Some (b.result)
            else
                Some (t)
        }

    /**
     * Traversal to one child.  Construct a strategy that applies `s` to the term
     * children of the subject term.  Assume that `c` is the
     * first child on which s succeeds.  Then stop applying `s` to the children and
     * succeed, forming a new term from the constructor of the original term and
     * the original children, except that `c` is replaced by the result of applying
     * `s` to `c`.  In the event that the strategy fails on all children, then fail.
     * If there are no children, fail.  If `s` succeeds on the one child producing
     * the same term (by `eq` for references and by `==` for other values), then
     * the overall strategy returns the subject term.
     * This operation works on instances of finite `Rewritable`, `Product`, `Map`
     * and `Traversable` values, checked for in that order.
     * Children of a `Rewritable` (resp. `Product`, collection) value are processed
     * in the order returned by the value's `deconstruct` (resp. `productElement`,
     * `foreach`) method.
     * `s` is evaluated at most once.
     */
    def one (s : Strategy) : Strategy =
        macro RewriterMacros.oneMacro

    /**
     * As for the other `one` with the first argument specifying a name for
     * the constructed strategy.
     */
    def one (n : String, s : => Strategy) : Strategy =
        new Strategy {

            val name = n

            lazy val strat = s

            def apply (t : Any) : Option[Any] =
                t match {
                    case r : Rewritable     => oneRewritable (strat, r)
                    case p : Product        => oneProduct (strat, p)
                    case m : Map[_,_]       => oneMap (strat, m.asInstanceOf[Map[Any,Any]])
                    case t : Traversable[_] => oneTraversable (strat, t.asInstanceOf[Traversable[Any]])
                    case _                  => None
                }

        }

    def oneProduct (s : Strategy, p : Product) : Option[Any] = {
        val numchildren = p.productArity
        var i = 0
        while (i < numchildren) {
            val ct = p.productElement (i)
            s (ct) match {
                case Some (ti) if (same (ct, ti)) =>
                    return Some (p)
                case Some (ti) =>
                    val newchildren = new Array[AnyRef] (numchildren)
                    var j = 0
                    while (j < i) {
                        newchildren (j) = makechild (p.productElement (j))
                        j = j + 1
                    }
                    newchildren (i) = makechild (ti)
                    j = j + 1
                    while (j < numchildren) {
                        newchildren (j) = makechild (p.productElement (j))
                        j = j + 1
                    }
                    val ret = dup (p, newchildren)
                    return Some (ret)
                case None =>
                    // Do nothing
            }
            i = i + 1
        }
        None
    }

    def oneRewritable (s : Strategy, r : Rewritable) : Option[Any] = {
        val numchildren = r.arity
        val children = r.deconstruct
        var i = 0
        while (i < numchildren) {
            val ct = children (i)
            s (ct) match {
                case Some (ti) if (same (ct, ti)) =>
                    return Some (r)
                case Some (ti) =>
                    val newchildren = new Array[Any] (numchildren)
                    var j = 0
                    while (j < i) {
                        newchildren (j) = makechild (children (j))
                        j = j + 1
                    }
                    newchildren (i) = makechild (ti)
                    j = j + 1
                    while (j < numchildren) {
                        newchildren (j) = makechild (children (j))
                        j = j + 1
                    }
                    val ret = r.reconstruct (newchildren)
                    return Some (ret)
                case None =>
                    // Do nothing
            }
            i = i + 1
        }
        None
    }

    def oneTraversable[CC[U] <: Traversable[U]] (s : Strategy, t : CC[Any])
                (implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]])
                        : Option[CC[Any]] = {
        val b = cbf (t)
        b.sizeHint (t.size)
        var add = true
        for (ct <- t)
            if (add)
                s (ct) match {
                    case Some (ti) if same (ct, ti) =>
                        return Some (t)
                    case Some (ti) =>
                        b += ti
                        add = false
                    case None =>
                        b += ct
                }
            else
                b += ct
        if (add)
            None
        else
            Some (b.result)
    }

    def oneMap[CC[V,W] <: Map[V,W]] (s : Strategy, t : CC[Any,Any])
                (implicit cbf : CanBuildFrom[CC[Any,Any], (Any, Any), CC[Any,Any]])
                        : Option[CC[Any,Any]] = {
        val b = cbf (t)
        b.sizeHint (t.size)
        var add = true
        for (ct <- t)
            if (add)
                s (ct) match {
                    case Some (ti @ (tix,tiy)) if (same (ct, ti)) =>
                        return Some (t)
                    case Some (ti @ (tix, tiy)) =>
                        b += ti
                        add = false
                    case None =>
                        b += ct
                }
            else
                b += ct
        if (add)
            None
        else
            Some (b.result)
    }

    /**
     * Traversal to as many children as possible, but at least one.  Construct a
     * strategy that applies `s` to the term children of the subject term.
     * If `s` succeeds on any of the children, then succeed,
     * forming a new term from the constructor of the original term and the result
     * of `s` for each succeeding child, with other children unchanged.  In the event
     * that the strategy fails on all children, then fail. If there are no
     * children, fail.  If `s` succeeds on children producing the same terms (by `eq`
     * for references and by `==` for other values), then the overall strategy
     * returns the subject term.
     * This operation works on instances of finite `Rewritable`, `Product`, `Map` and
     * `Traversable` values, checked for in that order.
     * Children of a `Rewritable` (resp. `Product`, collection) value are processed
     * in the order returned by the value's `deconstruct` (resp. `productElement`,
     * `foreach`) method.
     * `s` is evaluated at most once.
     */
    def some (s : Strategy) : Strategy =
        macro RewriterMacros.someMacro

    /**
     * As for the other `some` with the first argument specifying a name for
     * the constructed strategy.
     */
    def some (n : String, s : => Strategy) : Strategy =
        new Strategy {

            val name = n

            lazy val strat = s

            def apply (t : Any) : Option[Any] =
                t match {
                    case r : Rewritable     => someRewritable (strat, r)
                    case p : Product        => someProduct (strat, p)
                    case m : Map[_,_]       => someMap (strat, m.asInstanceOf[Map[Any,Any]])
                    case t : Traversable[_] => someTraversable (strat, t.asInstanceOf[Traversable[Any]])
                    case _                  => None
                }

        }

    def someProduct (s : Strategy, p : Product) : Option[Any] = {
        val numchildren = p.productArity
        if (numchildren == 0)
            None
        else {
            val newchildren = new Array[AnyRef](numchildren)
            var success = false
            var changed = false
            var i = 0
            while (i < numchildren) {
                val ct = p.productElement (i)
                s (ct) match {
                    case Some (ti) =>
                        newchildren (i) = makechild (ti)
                        if (!same (ct, ti))
                            changed = true
                        success = true
                    case None =>
                        newchildren (i) = makechild (ct)
                }
                i = i + 1
            }
            if (success)
                if (changed) {
                    val ret = dup (p, newchildren)
                    Some (ret)
                } else
                    Some (p)
            else
                None
        }
    }

    def someRewritable (s : Strategy, r : Rewritable) : Option[Any] = {
        val numchildren = r.arity
        if (numchildren == 0)
            None
        else {
            val children = r.deconstruct
            val newchildren = new Array[Any](numchildren)
            var success = false
            var changed = false
            var i = 0
            while (i < numchildren) {
                val ct = children (i)
                s (ct) match {
                    case Some (ti) =>
                        newchildren (i) = makechild (ti)
                        if (!same (ct, ti))
                            changed = true
                        success = true
                    case None =>
                        newchildren (i) = makechild (ct)
                }
                i = i + 1
            }
            if (success)
                if (changed) {
                    val ret = r.reconstruct (newchildren)
                    Some (ret)
                } else
                    Some (r)
            else
                None
        }
    }

    def someTraversable[CC[U] <: Traversable[U]] (s : Strategy, t : CC[Any])
                (implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]])
                        : Option[CC[Any]] =
        if (t.size == 0)
            None
        else {
            val b = cbf (t)
            b.sizeHint (t.size)
            var success = false
            var changed = false
            for (ct <- t)
                s (ct) match {
                    case Some (ti) =>
                        b += ti
                        if (!same (ct, ti))
                            changed = true
                        success = true
                    case None =>
                        b += ct
                }
            if (success)
                if (changed)
                    Some (b.result)
                else
                    Some (t)
            else
                None
        }

    def someMap[CC[V,W] <: Map[V,W]] (s : Strategy, t : CC[Any,Any])
                (implicit cbf : CanBuildFrom[CC[Any,Any], (Any, Any), CC[Any,Any]])
                        : Option[CC[Any,Any]] =
        if (t.size == 0)
            None
        else {
            val b = cbf (t)
            b.sizeHint (t.size)
            var success = false
            var changed = false
            for (ct <- t)
                s (ct) match {
                    case Some (ti @ (tix, tiy)) =>
                        b += ti
                        if (!same (ct, ti))
                            changed = true
                        success = true
                    case _ =>
                        b += ct
                }
            if (success)
                if (changed)
                    Some (b.result)
                else
                    Some (t)
            else
                None
        }

    /**
     * Make a strategy that applies the elements of ss pairwise to the
     * children of the subject term, returning a new term if all of the
     * strategies succeed, otherwise failing.  The constructor of the new
     * term is the same as that of the original term and the children
     * are the results of the strategies.  If the length of `ss` is not
     * the same as the number of children, then `congruence(ss)` fails.
     * If the argument strategies succeed on children producing the same
     * terms (by `eq` for references and by `==` for other values), then the
     * overall strategy returns the subject term.
     * This operation works on instances of `Product` values.
     */
    def congruence (ss : Strategy*) : Strategy =
        macro RewriterMacros.congruenceMacro

    /**
     * As for the other `congruence` with the first argument specifying a name for
     * the constructed strategy.
     */
    def congruence (n : String, ss : Strategy*) : Strategy =
        new Strategy {

            val name = n

            def apply (t : Any) : Option[Any] =
                t match {
                    case p : Product => congruenceProduct (p, ss : _*)
                    case _           => Some (t)
                }

        }

    def congruenceProduct (p : Product, ss : Strategy*) : Option[Any] = {
       val numchildren = p.productArity
       if (numchildren == ss.length) {
           val newchildren = new Array[AnyRef](numchildren)
           var changed = false
           var i = 0
           while (i < numchildren) {
               val ct = p.productElement (i)
               (ss (i)) (ct) match {
                   case Some (ti) =>
                       newchildren (i) = makechild (ti)
                       if (!same (ct, ti))
                           changed = true
                   case None =>
                       return None
               }
               i = i + 1
           }
           if (changed) {
               val ret = dup (p, newchildren)
               Some (ret)
           } else
               Some (p)
       } else
           None
    }

    // Extractors

    /**
     * Generic term deconstruction.
     */
    object Term {

        /**
         * Generic term deconstruction. An extractor that decomposes `Product`
         * or `Rewritable` values into the value itself and a sequence of its
         * children.  Terms that are not `Product` or `Rewritable` are not
         * decomposable (i.e., the list of children will be empty).
         */
        def unapply (t : Any) : Option[(Any,Seq[Any])] = {
            t match {
                case r : Rewritable =>
                    Some ((r, r.deconstruct))
                case p : Product =>
                    val cs = for (i <- 0 until p.productArity) yield p.productElement (i)
                    Some ((p, cs))
                case _ =>
                    Some ((t, Nil))
            }
        }

    }

}
