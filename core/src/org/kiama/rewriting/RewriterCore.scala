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
 * basic strategies and rewrite rules, plus basic library combinators.
 */
trait RewriterCore {

    import org.kiama.util.Emitter
    import scala.collection.generic.CanBuildFrom
    import scala.collection.mutable.WeakHashMap
    import scala.collection.immutable.Seq
    import scala.language.higherKinds
    import scala.language.experimental.macros

    /**
     * Make a strategy with the given name and body `f`. By default, make a
     * basic strategy.
     */
    def mkStrategy (name : String, f : Any => Option[Any]) : Strategy =
        new Strategy (name) {
            val body = f
        }

    // Builder combinators.

    /**
     * Construct a strategy that always succeeds, changing the subject term to
     * the given term `t`. The term `t` is evaluated at most once.
     */
    def build (t : Any) : Strategy =
        macro RewriterCoreMacros.buildMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def build (name : String, t : => Any) : Strategy =
        rulef (name, _ => t)

    /**
     * A strategy that always succeeds with the subject term unchanged (i.e.,
     * this is the identity strategy) with the side-effect that the subject
     * term is printed to the given emitter, prefixed by the string `s`.  The
     * emitter defaults to one that writes to standard output.
     */
    def debug (msg : String, emitter : Emitter = new Emitter) : Strategy =
        macro RewriterCoreMacros.debugMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def debug (name : String, msg : String, emitter : Emitter) : Strategy =
        strategyf (name, t => { emitter.emitln (msg + t); Some (t) })

    /**
     * A strategy that always fails.
     */
    val fail : Strategy =
        mkStrategy ("fail", _ => None)

    /**
     * A strategy that always succeeds.
     */
    val id : Strategy =
        mkStrategy ("id", Some (_))

    /**
     * Create a logging strategy based on a strategy `s`. The returned strategy
     * succeeds or fails exactly as `s` does, but also prints the provided message,
     * the subject term, the success or failure status, and on success, the result
     * term, to the provided emitter (default: standard output). `s` is evaluated
     * at most once.
     */
    def log (s : Strategy, msg : String, emitter : Emitter = new Emitter) : Strategy =
        macro RewriterCoreMacros.logMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def log (name : String, s : => Strategy, msg : String, emitter : Emitter) : Strategy = {
        lazy val strat = s
        mkStrategy (name,
            t1 => {
                emitter.emit (msg + t1)
                val r = strat (t1)
                r match {
                    case Some (t2) =>
                        emitter.emitln (s" succeeded with $t2")
                    case None =>
                        emitter.emitln (" failed")
                }
                r
            }
        )
    }

    /**
     * Create a logging strategy based on a strategy `s`.  The returned strategy
     * succeeds or fails exactly as `s` does, but if `s` fails, also prints the
     * provided message and the subject term to the provided emitter (default:
     * standard output). `s` is evaluated at most once.
     */
    def logfail[T] (s : Strategy, msg : String, emitter : Emitter = new Emitter) : Strategy =
        macro RewriterCoreMacros.logfailMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def logfail[T] (name : String, s : => Strategy, msg : String, emitter : Emitter) : Strategy = {
        lazy val strat = s
        mkStrategy (name,
            t1 => {
                val r = strat (t1)
                r match {
                    case Some (t2) =>
                        // Do nothing
                    case None =>
                        emitter.emitln (s"$msg$t1 failed")
                }
                r
            }
        )
    }

    /**
     * Return a strategy that behaves as `s` does, but memoises its arguments and
     * results.  In other words, if `memo(s)` is called on a term `t` twice, the
     * second time will return the same result as the first, without having to
     * invoke `s`.  For best results, it is important that `s` should have no side
     * effects. `s` is evaluated at most once.
     */
    def memo (s : Strategy) : Strategy =
        macro RewriterCoreMacros.memoMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def memo (name : String, s : => Strategy) : Strategy = {
        lazy val strat = s
        val cache = new scala.collection.mutable.HashMap[Any,Option[Any]]
        mkStrategy (name, t => cache.getOrElseUpdate (t, strat (t)))
    }

    /**
     * Construct a strategy from an option value `o`. The strategy succeeds
     * or fails depending on whether `o` is a Some or None, respectively.
     * If `o` is a `Some`, then the subject term is changed to the term that
     * is wrapped by the `Some`. `o` is evaluated at most once.
     */
    def option (o : Option[Any]) : Strategy =
        macro RewriterCoreMacros.optionMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def option (name : String, o : => Option[Any]) : Strategy =
        strategyf (name, _ => o)

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
        macro RewriterCoreMacros.queryMacro[T]

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def query[T] (name : String, f : Any ==> T) : Strategy =
        mkStrategy (name,
            t => {
                if (f isDefinedAt t)
                    f (t)
                Some (t)
            }
        )

    /**
     * Define a term query by a function `f`. The query always succeeds with
     * no effect on the subject term but applies the given (possibly partial)
     * function `f` to the subject term.  In other words, the strategy runs
     * `f` for its side-effects.
     */
    def queryf[T] (f : Any => T) : Strategy =
        macro RewriterCoreMacros.queryfMacro[T]

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def queryf[T] (name : String, f : Any => T) : Strategy =
        mkStrategy (name,
            t => {
                f (t)
                Some (t)
            }
        )

    /**
     * Define a rewrite rule using a partial function `f`. If the function is
     * defined at the current term, then the strategy succeeds with the return
     * value of the function applied to the current term. Otherwise the
     * strategy fails.
     */
    def rule (f : Any ==> Any) : Strategy =
        macro RewriterCoreMacros.ruleMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def rule (name : String, f : Any ==> Any) : Strategy =
        mkStrategy (name,
            t =>
                if (f isDefinedAt t)
                    Some (f (t))
                else
                    None
        )

    /**
     * Define a rewrite rule using a function `f` that returns a term.
     * The rule always succeeds with the return value of the function.
     */
    def rulef (f : Any => Any) : Strategy =
        macro RewriterCoreMacros.rulefMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def rulef (name : String, f : Any => Any) : Strategy =
        strategyf (name, t => Some (f (t)))

    /**
     * Define a rewrite rule using a function `f` that returns a strategy.  The
     * rule applies the function to the subject term to get a strategy which
     * is then applied again to the subject term. In other words, the function
     * is only used for effects such as pattern matching.  The whole thing also
     * fails if `f` is not defined at the term in the first place.
     */
    def rulefs (f : Any ==> Strategy) : Strategy =
        macro RewriterCoreMacros.rulefsMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def rulefs (name : String, f : Any ==> Strategy) : Strategy =
        mkStrategy (name,
            t =>
                if (f isDefinedAt t)
                    (f (t)) (t)
                else
                    None
        )

    /**
     * Make a strategy from a partial function `f`. If the function is
     * defined at the current term, then the function return value
     * when applied to the current term determines whether the strategy
     * succeeds or fails. If the function is not defined at the current
     * term, the strategy fails.
     */
    def strategy (f : Any ==> Option[Any]) : Strategy =
        macro RewriterCoreMacros.strategyMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def strategy (name : String, f : Any ==> Option[Any]) : Strategy =
        mkStrategy (name,
            t =>
                if (f isDefinedAt t)
                    f (t)
                else
                    None
        )

    /**
     * Make a strategy from a function `f`. The function return value
     * determines whether the strategy succeeds or fails.
     */
    def strategyf (f : Any => Option[Any]) : Strategy =
        macro RewriterCoreMacros.strategyfMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def strategyf (name : String, f : Any => Option[Any]) : Strategy =
        mkStrategy (name, f)

    /**
     * Construct a strategy that succeeds only if the subject term matches
     * the given term `t`.
     */
    def term (t : Any) : Strategy =
        macro RewriterCoreMacros.termMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def term (name : String, t : Any) : Strategy =
        rule (name, {
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
    protected def dup[T <: Product] (t : T, children : Seq[AnyRef]) : T = {
        val clazz = t.getClass
        val ctor = constrcache.getOrElseUpdate (clazz, (clazz.getConstructors())(0))
        try {
            ctor.newInstance (children : _*).asInstanceOf[T]
        } catch {
            case e : IllegalArgumentException =>
                sys.error (s"""dup illegal arguments: $ctor (${children.mkString (",")}), expects ${ctor.getParameterTypes.length}""")
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
        macro RewriterCoreMacros.childMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def child (name : String, i : Int, s : => Strategy) : Strategy =
        mkStrategy (name, {
            lazy val strat = s
            t =>
                t match {
                    case p : Product => childProduct (strat, i, p)
                    case t : Seq[_]  => childSeq (strat, i, t.asInstanceOf[Seq[Any]])
                    case _           => None
                }
        })

    /**
     * Implementation of `child` for `Product` values.
     */
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
                    val newchildren = p.productIterator.toArray.map (makechild)
                    newchildren (i - 1) = makechild (ti)
                    val ret = dup (p, newchildren.toIndexedSeq)
                    Some (ret)
                case None =>
                    None
            }
        }
    }

    /**
     * Implementation of `child` for `Seq` values.
     */
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
                    t.foldLeft (0) {
                        case (j, ct) =>
                            b += (if (j == i - 1) ti else ct)
                            j + 1
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
                    sys.error (s"Rewriter.same: comparison of non-AnyRefs $v1 and $v2, should not be reached")
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
        macro RewriterCoreMacros.allMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def all (name : String, s : => Strategy) : Strategy =
        mkStrategy (name, {
            lazy val strat = s
            t =>
                t match {
                    case r : Rewritable     => allRewritable (strat, r)
                    case p : Product        => allProduct (strat, p)
                    case m : Map[_,_]       => allMap (strat, m.asInstanceOf[Map[Any,Any]])
                    case t : Traversable[_] => allTraversable (strat, t.asInstanceOf[Traversable[Any]])
                    case _                  => Some (t)
                }
        })

    /**
     * Implementation of `all` for `Rewritable` values.
     */
    def allRewritable (s : Strategy, r : Rewritable) : Option[Any] = {
        val numchildren = r.arity
        if (numchildren == 0)
            Some (r)
        else {
            val newchildren = Seq.newBuilder[Any]
            val changed =
                r.deconstruct.foldLeft (false) {
                    case (changed, ct) =>
                        s (ct) match {
                            case Some (ti) =>
                                newchildren += makechild (ti)
                                changed || !same (ct, ti)
                            case None =>
                                return None
                        }
                }
            if (changed) {
                val ret = r.reconstruct (newchildren.result ())
                Some (ret)
            } else
                Some (r)
        }
    }

    /**
     * Implementation of `all` for `Product` values.
     */
    def allProduct (s : Strategy, p : Product) : Option[Any] = {
        val numchildren = p.productArity
        if (numchildren == 0)
            Some (p)
        else {
            val newchildren = Seq.newBuilder[AnyRef]
            val changed =
                p.productIterator.foldLeft (false) {
                    case (changed, ct) =>
                        s (ct) match {
                            case Some (ti) =>
                                newchildren += makechild (ti)
                                changed || !same (ct, ti)
                            case None =>
                                return None
                        }
                }
            if (changed) {
                val ret = dup (p, newchildren.result)
                Some (ret)
            } else
                Some (p)
        }
    }

    /**
     * Implementation of `all` for `Traversable` values.
     */
    def allTraversable[CC[_] <: Traversable[Any]] (s : Strategy, t : CC[Any])
            (implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]])
                        : Option[CC[Any]] =
        if (t.size == 0)
            Some (t)
        else {
            val b = cbf (t)
            b.sizeHint (t.size)
            val (changed, _) =
                t.foldLeft (false, 0) {
                    case ((changed, i), ct) =>
                        s (ct) match {
                            case Some (ti) =>
                                b += ti
                                (changed || !same (ct, ti), i + 1)
                            case None =>
                                return None
                        }
                }
            if (changed)
                Some (b.result)
            else
                Some (t)
        }

    /**
     * Implementation of `all` for `Map` values.
     */
    def allMap[CC[V,W] <: Map[V,W]] (s : Strategy, t : CC[Any,Any])
            (implicit cbf : CanBuildFrom[CC[Any,Any], (Any, Any), CC[Any,Any]])
                        : Option[CC[Any,Any]] =
        if (t.size == 0)
            Some (t)
        else {
            val b = cbf (t)
            b.sizeHint (t.size)
            val (changed, _) =
                t.foldLeft (false, 0) {
                    case ((changed, i), ct) =>
                        s (ct) match {
                            case Some (ti @ (tix,tiy)) =>
                                b += ti
                                (changed || !same (ct, ti), i + 1)
                            case _ =>
                                return None
                        }
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
        macro RewriterCoreMacros.oneMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def one (name : String, s : => Strategy) : Strategy =
        mkStrategy (name, {
            lazy val strat = s
            t =>
                t match {
                    case r : Rewritable     => oneRewritable (strat, r)
                    case p : Product        => oneProduct (strat, p)
                    case m : Map[_,_]       => oneMap (strat, m.asInstanceOf[Map[Any,Any]])
                    case t : Traversable[_] => oneTraversable (strat, t.asInstanceOf[Traversable[Any]])
                    case _                  => None
                }
        })

    /**
     * Implementation of `one` for `Rewritable` values.
     */
    def oneRewritable (s : Strategy, r : Rewritable) : Option[Any] = {
        val numchildren = r.arity
        val children = r.deconstruct
        children.foldLeft (0) {
            case (i, ct) =>
                s (ct) match {
                    case Some (ti) if (same (ct, ti)) =>
                        return Some (r)
                    case Some (ti) =>
                        val newchildren = children.updated (i, ti)
                        val ret = r.reconstruct (newchildren)
                        return Some (ret)
                    case None =>
                        i + 1
                }
        }
        None
    }

    /**
     * Implementation of `one` for `Product` values.
     */
    def oneProduct (s : Strategy, p : Product) : Option[Any] = {
        val numchildren = p.productArity
        p.productIterator.foldLeft (0) {
            case (i, ct) =>
                s (ct) match {
                    case Some (ti) if (same (ct, ti)) =>
                        return Some (p)
                    case Some (ti) =>
                        val newchildren = p.productIterator.toArray.map (makechild)
                        newchildren (i) = makechild (ti)
                        val ret = dup (p, newchildren.toIndexedSeq)
                        return Some (ret)
                    case None =>
                        i + 1
                }
        }
        None
    }

    /**
     * Implementation of `one` for `Traversable` values.
     */
    def oneTraversable[CC[U] <: Traversable[U]] (s : Strategy, t : CC[Any])
                (implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]])
                        : Option[CC[Any]] = {
        val b = cbf (t)
        b.sizeHint (t.size)
        val add =
            t.foldLeft (true) {
                case (add, ct) =>
                    if (add)
                        s (ct) match {
                            case Some (ti) if same (ct, ti) =>
                                return Some (t)
                            case Some (ti) =>
                                b += ti
                                false
                            case None =>
                                b += ct
                                true
                        }
                    else {
                        b += ct
                        false
                    }
            }
        if (add)
            None
        else
            Some (b.result)
    }

    /**
     * Implementation of `one` for `Map` values.
     */
    def oneMap[CC[V,W] <: Map[V,W]] (s : Strategy, t : CC[Any,Any])
                (implicit cbf : CanBuildFrom[CC[Any,Any], (Any, Any), CC[Any,Any]])
                        : Option[CC[Any,Any]] = {
        val b = cbf (t)
        b.sizeHint (t.size)
        val add =
            t.foldLeft (true) {
                case (add, ct) =>
                    if (add)
                        s (ct) match {
                            case Some (ti @ (tix,tiy)) if (same (ct, ti)) =>
                                return Some (t)
                            case Some (ti @ (tix, tiy)) =>
                                b += ti
                                false
                            case None =>
                                b += ct
                                true
                        }
                    else {
                        b += ct
                        false
                    }
            }
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
        macro RewriterCoreMacros.someMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def some (name : String, s : => Strategy) : Strategy =
        mkStrategy (name, {
            lazy val strat = s
            t =>
                t match {
                    case r : Rewritable     => someRewritable (strat, r)
                    case p : Product        => someProduct (strat, p)
                    case m : Map[_,_]       => someMap (strat, m.asInstanceOf[Map[Any,Any]])
                    case t : Traversable[_] => someTraversable (strat, t.asInstanceOf[Traversable[Any]])
                    case _                  => None
                }
        })

    /**
     * Implementation of `some` for `Rewritable` values.
     */
    def someRewritable (s : Strategy, r : Rewritable) : Option[Any] = {
        val numchildren = r.arity
        if (numchildren == 0)
            None
        else {
            val newchildren = Seq.newBuilder[Any]
            val (success, changed) =
                r.deconstruct.foldLeft (false, false) {
                    case ((success, changed), ct) =>
                        s (ct) match {
                            case Some (ti) =>
                                newchildren += makechild (ti)
                                (true, changed || !same (ct, ti))
                            case None =>
                                newchildren += makechild (ct)
                                (success, changed)
                        }
                }
            if (success)
                if (changed) {
                    val ret = r.reconstruct (newchildren.result ())
                    Some (ret)
                } else
                    Some (r)
            else
                None
        }
    }

    /**
     * Implementation of `some` for `Product` values.
     */
    def someProduct (s : Strategy, p : Product) : Option[Any] = {
        val numchildren = p.productArity
        if (numchildren == 0)
            None
        else {
            val newchildren = Seq.newBuilder[AnyRef]
            val (success, changed) =
                p.productIterator.foldLeft (false, false) {
                    case ((success, changed), ct) =>
                        s (ct) match {
                            case Some (ti) =>
                                newchildren += makechild (ti)
                                (true, changed || !same (ct, ti))
                            case None =>
                                newchildren += makechild (ct)
                                (success, changed)
                        }
                }
            if (success)
                if (changed) {
                    val ret = dup (p, newchildren.result)
                    Some (ret)
                } else
                    Some (p)
            else
                None
        }
    }

    /**
     * Implementation of `some` for `Traversable` values.
     */
    def someTraversable[CC[U] <: Traversable[U]] (s : Strategy, t : CC[Any])
                (implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]])
                        : Option[CC[Any]] =
        if (t.size == 0)
            None
        else {
            val b = cbf (t)
            b.sizeHint (t.size)
            val (success, changed) =
                t.foldLeft (false, false) {
                    case ((success, changed), ct) =>
                        s (ct) match {
                            case Some (ti) =>
                                b += ti
                                (true, changed || !same (ct, ti))
                            case None =>
                                b += ct
                                (success, changed)
                        }
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
     * Implementation of `some` for `Map` values.
     */
    def someMap[CC[V,W] <: Map[V,W]] (s : Strategy, t : CC[Any,Any])
                (implicit cbf : CanBuildFrom[CC[Any,Any], (Any, Any), CC[Any,Any]])
                        : Option[CC[Any,Any]] =
        if (t.size == 0)
            None
        else {
            val b = cbf (t)
            b.sizeHint (t.size)
            val (success, changed) =
                t.foldLeft (false, false) {
                    case ((success, changed), ct) =>
                        s (ct) match {
                            case Some (ti @ (tix, tiy)) =>
                                b += ti
                                (true, changed || !same (ct, ti))
                            case _ =>
                                b += ct
                                (success, changed)
                        }
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
        macro RewriterCoreMacros.congruenceMacro

    /**
     * As for the version without the `name` argument but specifies the name for
     * the constructed strategy.
     */
    def congruence (name : String, ss : Strategy*) : Strategy =
        mkStrategy (name,
            t =>
                t match {
                    case p : Product => congruenceProduct (p, ss : _*)
                    case _           => Some (t)
                }
        )

    /**
     * Implementation of `congruence` for `Product` values.
     */
    def congruenceProduct (p : Product, ss : Strategy*) : Option[Any] = {
       val numchildren = p.productArity
       if (numchildren == ss.length) {
           val newchildren = Seq.newBuilder[AnyRef]
           val (changed, _) =
               p.productIterator.foldLeft (false, 0) {
                   case ((changed, i), ct) =>
                       (ss (i)) (ct) match {
                           case Some (ti) =>
                               newchildren += makechild (ti)
                               (changed || !same (ct, ti), i + 1)
                           case None =>
                               return None
                       }
               }
           if (changed) {
               val ret = dup (p, newchildren.result)
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

    // Library combinators

    /**
     * Construct a strategy that applies `s` in a bottom-up fashion to all
     * subterms at each level, stopping at a frontier where s succeeds.
     */
    def allbu (s : Strategy) : Strategy =
        macro RewriterCoreMacros.allbuMacro

    /**
     * Construct a strategy that applies `s` in a top-down fashion, stopping
     * at a frontier where s succeeds.
     */
    def alltd (s : Strategy) : Strategy =
        macro RewriterCoreMacros.alltdMacro

    /**
     * Construct a strategy that applies `s1` in a top-down, prefix fashion
     * stopping at a frontier where `s1` succeeds.  `s2` is applied in a bottom-up,
     * postfix fashion to the result.
     */
    def alldownup2 (s1 : Strategy, s2 : Strategy) : Strategy =
        macro RewriterCoreMacros.alldownup2Macro

    /**
     * Construct a strategy that applies `s1` in a top-down, prefix fashion
     * stopping at a frontier where `s1` succeeds.  `s2` is applied in a bottom-up,
     * postfix fashion to the results of the recursive calls.
     */
    def alltdfold (s1 : Strategy, s2 : Strategy) : Strategy =
        macro RewriterCoreMacros.alltdfoldMacro

    /**
     * `and(s1, s2)` applies `s1` and `s2` to the current
     * term and succeeds if both succeed. `s2` will always
     * be applied, i.e., and is ''not'' a short-circuit
     * operator
     */
    def and (s1 : Strategy, s2 : Strategy) : Strategy =
        macro RewriterCoreMacros.andMacro

    /**
     * Construct a strategy that applies `s`, yielding the result of `s` if it
     * succeeds, otherwise leave the original subject term unchanged.  In
     * Stratego library this strategy is called `try`.
     */
    def attempt (s : Strategy) : Strategy =
        macro RewriterCoreMacros.attemptMacro

    /**
     * Construct a strategy that applies `s` in a bottom-up, postfix fashion
     * to the subject term.
     */
    def bottomup (s : Strategy) : Strategy =
        macro RewriterCoreMacros.bottomupMacro

    /**
     * Construct a strategy that applies `s` in a bottom-up, postfix fashion
     * to the subject term but stops when the strategy produced by `stop`
     * succeeds. `stop` is given the whole strategy itself as its argument.
     */
    def bottomupS (s : Strategy, stop : (=> Strategy) => Strategy) : Strategy =
        macro RewriterCoreMacros.bottomupSMacro

    /**
     * Construct a strategy that applies `s` in breadth first order. This
     * strategy does not apply `s` to the root of the subject term.
     *
     * It is called `breadthfirst` to follow Stratego's library, but is not
     * really conducting a breadth-first traversal since all of the
     * descendants of the first child of a term are visited before any of
     * the descendants of the second child of a term.
     */
    def breadthfirst (s : Strategy) : Strategy =
        macro RewriterCoreMacros.breadthfirstMacro

    /**
     * Construct a strategy that applies `s` at least once and then repeats `s`
     * while `r` succeeds.  This operator is called `do-while` in the Stratego
     * library.
     */
    def doloop (s : Strategy, r : Strategy) : Strategy =
        macro RewriterCoreMacros.doloopMacro

    /**
     * Construct a strategy that applies `s` in a combined top-down and
     * bottom-up fashion (i.e., both prefix and postfix) to the subject
     * term.
     */
    def downup (s : Strategy) : Strategy =
        macro RewriterCoreMacros.downupMacro1

    /**
     * Construct a strategy that applies `s1` in a top-down, prefix fashion
     * and `s2` in a bottom-up, postfix fashion to the subject term.
     */
    def downup (s1 : Strategy, s2 : Strategy) : Strategy =
        macro RewriterCoreMacros.downupMacro2

    /**
     * Construct a strategy that applies `s` in a combined top-down and
     * bottom-up fashion (i.e., both prefix and postfix) to the subject
     * but stops when the strategy produced by `stop` succeeds. `stop` is
     * given the whole strategy itself as its argument.
     */
    def downupS (s : Strategy, stop : (=> Strategy) => Strategy) : Strategy =
        macro RewriterCoreMacros.downupSMacro1

    /**
     * Construct a strategy that applies `s1` in a top-down, prefix fashion
     * and `s2` in a bottom-up, postfix fashion to the subject term but stops
     * when the strategy produced by `stop` succeeds. `stop` is given the whole
     * strategy itself as its argument.
     */
    def downupS (s1 : Strategy, s2 : Strategy, stop : (=> Strategy) => Strategy) : Strategy =
        macro RewriterCoreMacros.downupSMacro2

    /**
     * Same as `everywheretd`.
     */
    def everywhere (s : Strategy) : Strategy =
        macro RewriterCoreMacros.everywhereMacro

    /**
     * Construct a strategy that applies `s` at all terms in a bottom-up fashion
     * regardless of failure.  Terms for which the strategy fails are left
     * unchanged.
     */
    def everywherebu (s : Strategy) : Strategy =
        macro RewriterCoreMacros.everywherebuMacro

    /**
     * Construct a strategy that applies `s` at all terms in a top-down fashion
     * regardless of failure.  Terms for which the strategy fails are left
     * unchanged.
     */
    def everywheretd (s : Strategy) : Strategy =
        macro RewriterCoreMacros.everywheretdMacro

    /**
     * Construct a strategy that applies `s` repeatedly to the innermost
     * (i.e., lowest and left-most) (sub-)term to which it applies.
     * Stop with the current term if `s` doesn't apply anywhere.
     */
    def innermost (s : Strategy) : Strategy =
        macro RewriterCoreMacros.innermostMacro

    /**
     * An alternative version of `innermost`.
     */
    def innermost2 (s : Strategy) : Strategy =
        macro RewriterCoreMacros.innermost2Macro

    /**
     * `ior(s1, s2)` implements inclusive OR, that is, the
     * inclusive choice of `s1` and `s2`. It first tries `s1`. If
     * that fails it applies `s2` (just like `s1 <+ s2`). However,
     * when `s1` succeeds it also tries to apply `s2`.
     */
    def ior (s1 : Strategy, s2 : Strategy) : Strategy =
        macro RewriterCoreMacros.iorMacro

    /**
     * Applies `s` followed by `f` whether `s` failed or not.
     * This operator is called `finally` in the Stratego library.
     */
    def lastly (s : Strategy, f : Strategy) : Strategy =
        macro RewriterCoreMacros.lastlyMacro

    /**
     * Construct a strategy that applies to all of the leaves of the
     * current term, using `isleaf` as the leaf predicate.
     */
    def leaves (s : Strategy, isleaf : Strategy) : Strategy =
        macro RewriterCoreMacros.leavesMacro1

    /**
     * Construct a strategy that applies to all of the leaves of the
     * current term, using `isleaf` as the leaf predicate, skipping
     * subterms for which `skip` when applied to the result succeeds.
     */
    def leaves (s : Strategy, isleaf : Strategy, skip : Strategy => Strategy) : Strategy =
        macro RewriterCoreMacros.leavesMacro2

    /**
     * Construct a strategy that while `r` succeeds applies `s`.  This operator
     * is called `while` in the Stratego library.
     */
    def loop (r : Strategy, s : Strategy) : Strategy =
        macro RewriterCoreMacros.loopMacro

    /**
     * Construct a strategy that repeats application of `s` while `r` fails, after
     * initialization with `i`.  This operator is called `for` in the Stratego
     * library.
     */
    def loopiter (i : Strategy, r : Strategy, s : Strategy) : Strategy =
        macro RewriterCoreMacros.loopiterMacro1

    /**
     * Construct a strategy that applies `s(i)` for each integer `i` from `low` to
     * `high` (inclusive).  This operator is called `for` in the Stratego library.
     */
    def loopiter (s : Int => Strategy, low : Int, high : Int) : Strategy =
        macro RewriterCoreMacros.loopiterMacro2

    /**
     * Construct a strategy that while `r` does not succeed applies `s`.  This
     * operator is called `while-not` in the Stratego library.
     */
    def loopnot (r : Strategy, s : Strategy) : Strategy =
        macro RewriterCoreMacros.loopnotMacro
    /**
     * Construct a strategy that applies `s` to each element of a sequence,
     * returning a new sequence of the results if all of the applications
     * succeed, otherwise fail.  If all of the applications succeed
     * without change, return the input sequence.
     */
    def map (s : Strategy) : Strategy =
        macro RewriterCoreMacros.mapMacro

    /**
     * Construct a strategy that applies `s` as many times as possible, but
     * at least once, in bottom up order.
     */
    def manybu (s : Strategy) : Strategy =
        macro RewriterCoreMacros.manybuMacro

    /**
     * Construct a strategy that applies `s` as many times as possible, but
     * at least once, in top down order.
     */
    def manytd (s : Strategy) : Strategy =
        macro RewriterCoreMacros.manytdMacro

    /**
     * Construct a strategy that applies `s`, then fails if `s` succeeded or, if `s`
     * failed, succeeds with the subject term unchanged,  I.e., it tests if
     * `s` applies, but has no effect on the subject term.
     */
    def not (s : Strategy) : Strategy =
        macro RewriterCoreMacros.notMacro

    /**
     * Construct a strategy that applies `s` in a bottom-up fashion to one
     * subterm at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def oncebu (s : Strategy) : Strategy =
        macro RewriterCoreMacros.oncebuMacro

    /**
     * Construct a strategy that applies `s` in a top-down fashion to one
     * subterm at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def oncetd (s : Strategy) : Strategy =
        macro RewriterCoreMacros.oncetdMacro

    /**
     * `or(s1, s2)` is similar to `ior(s1, s2)`, but the application
     * of the strategies is only tested.
     */
    def or (s1 : Strategy, s2 : Strategy) : Strategy =
        macro RewriterCoreMacros.orMacro

    /**
     * Construct a strategy that applies `s` repeatedly in a top-down fashion
     * stopping each time as soon as it succeeds once (at any level). The
     * outermost fails when `s` fails to apply to any (sub-)term.
     */
    def outermost (s : Strategy) : Strategy =
        macro RewriterCoreMacros.outermostMacro

    /**
     * Construct a strategy that applies `s` repeatedly to subterms
     * until it fails on all of them.
     */
    def reduce (s : Strategy) : Strategy =
        macro RewriterCoreMacros.reduceMacro

    /**
     * Construct a strategy that applies `s` repeatedly until it fails.
     */
    def repeat (s : Strategy) : Strategy =
        macro RewriterCoreMacros.repeatMacro1

    /**
     * Construct a strategy that repeatedly applies `s` until it fails and
     * then terminates with application of `r`.
     */
    def repeat (s : Strategy, r : Strategy) : Strategy =
        macro RewriterCoreMacros.repeatMacro2

    /**
     * Construct a strategy that applies `s` repeatedly exactly `n` times. If
     * `s` fails at some point during the n applications, the entire strategy
     * fails. The result of the strategy is that of the ''nth'' application of
     * `s`.
     */
    def repeat (s : Strategy, n : Int) : Strategy =
        macro RewriterCoreMacros.repeatMacro3

    /**
     * Construct a strategy that repeatedly applies `s` (at least once).
     */
    def repeat1 (s : Strategy) : Strategy =
        macro RewriterCoreMacros.repeat1Macro1

    /**
     * Construct a strategy that repeatedly applies `s` (at least once) and
     * terminates with application of `c`.
     */
    def repeat1 (s : Strategy, r : Strategy) : Strategy =
        macro RewriterCoreMacros.repeat1Macro2

    /**
     * Construct a strategy that repeatedly applies `s` until `c` succeeds.
     */
    def repeatuntil (s : Strategy, r : Strategy) : Strategy =
        macro RewriterCoreMacros.repeatuntilMacro

    /**
     * Construct a strategy that applies `s`, then applies the restoring action
     * `rest` if `s` fails (and then fail). Otherwise, let the result of `s` stand.
     * Typically useful if `s` performs side effects that should be restored or
     * undone when `s` fails.
     */
    def restore (s : Strategy, rest : Strategy) : Strategy =
        macro RewriterCoreMacros.restoreMacro

    /**
     * Construct a strategy that applies `s`, then applies the restoring action
     * `rest` regardless of the success or failure of `s`. The whole strategy
     * preserves the success or failure of `s`. Typically useful if `s` performs
     * side effects that should be restored always, e.g., when maintaining scope
     * information.
     */
    def restorealways (s : Strategy, rest : Strategy) : Strategy =
        macro RewriterCoreMacros.restoreAlwaysMacro

    /**
     * Construct a strategy that applies `s` in a bottom-up fashion to some
     * subterms at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def somebu (s : Strategy) : Strategy =
        macro RewriterCoreMacros.somebuMacro

    /**
     * Construct a strategy that applies `s` in a top-down, prefix fashion
     * stopping at a frontier where `s` succeeds on some children.  `s` is then
     * applied in a bottom-up, postfix fashion to the result.
     */
    def somedownup (s : Strategy) : Strategy =
        macro RewriterCoreMacros.somedownupMacro

    /**
     * Construct a strategy that applies `s` in a top-down fashion to some
     * subterms at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def sometd (s : Strategy) : Strategy =
        macro RewriterCoreMacros.sometdMacro

    /**
     * Construct a strategy that tests whether strategy `s` succeeds,
     * restoring the original term on success.  A synonym for `where`.
     */
    def test (s : Strategy) : Strategy =
        macro RewriterCoreMacros.testMacro

    /**
     * Construct a strategy that applies `s` in a top-down, prefix fashion
     * to the subject term.
     */
    def topdown (s : Strategy) : Strategy =
        macro RewriterCoreMacros.topdownMacro

    /**
     * Construct a strategy that applies `s` in a top-down, prefix fashion
     * to the subject term but stops when the strategy produced by `stop`
     * succeeds. `stop` is given the whole strategy itself as its argument.
     */
    def topdownS (s : Strategy, stop : (=> Strategy) => Strategy) : Strategy =
        macro RewriterCoreMacros.topdownSMacro

    /**
     * Construct a strategy that tests whether strategy `s` succeeds,
     * restoring the original term on success.  This is similar
     * to Stratego's `where`, except that in this version any effects on
     * bindings are not visible outside `s`.
     */
    def where (s : Strategy) : Strategy =
        macro RewriterCoreMacros.whereMacro

}
