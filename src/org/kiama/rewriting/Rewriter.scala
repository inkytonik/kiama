/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2012 Anthony M Sloane, Macquarie University.
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
 * Strategy-based term rewriting in the style of Stratego (http://strategoxt.org/).
 * The implementation here is partially based on the semantics given in "Program
 * Transformation with Scoped Dynamic Rewrite Rules", by Bravenboer, van Dam, Olmos
 * and Visser, Fundamenta Informaticae, 69, 2005. The library strategies are mostly
 * based on the Stratego library, but also on combinators found in the Scrap Your
 * Boilerplate and Uniplate libraries for Haskell.
 */
class Rewriter {
    
    import language.higherKinds
    import org.kiama.util.Emitter
    import scala.collection.generic.CanBuildFrom
    import scala.collection.mutable.Builder
    import scala.collection.mutable.WeakHashMap

    /**
     * The type of terms that can be rewritten.  Any type of value is acceptable
     * but generic traversals will only work on some specific types.  See the 
     * documentation of the specific generic traversals (e.g., `all` or `some`)
     * for a detailed description.
     */
    type Term = Any

    /**
     * Term-rewriting strategies. A strategy is a function that takes a term
     * as input and either succeeds producing a new term (`Some`), or fails
     * (`None`).
     */
    abstract class Strategy extends (Term => Option[Term]) {

        /**
         * Alias this strategy as `p` to make it easier to refer to in the
         * combinator definitions.
         */
        p =>

        /**
         * Apply this strategy to a term, producing either a transformed term
         * wrapped in `Some`, or `None`, representing a rewriting failure.
         */
        def apply (r : Term) : Option[Term]

        /**
         * Sequential composition. Construct a strategy that first applies
         * this strategy. If it succeeds, then apply `q` to the new subject
         * term. Otherwise fail.
         */
        def <* (q : => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => q (t2)
                        case None      => None
                    }
            }

        /**
         * Deterministic choice.  Construct a strategy that first applies
         * this strategy. If it succeeds, succeed with the resulting term.
         * Otherwise, apply `q` to the original subject term.
         */
        def <+ (q : => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => Some (t2)
                        case None      => q (t1)
                    }
            }

        /**
         * Non-deterministic choice. Normally, construct a strategy that
         * first applies either this strategy or the given strategy. If it
         * succeeds, succeed with the resulting term. Otherwise, apply `q`.
         * Currently implemented as deterministic choice, but this behaviour
         * should not be relied upon.
         * When used as the argument to the `<` conditional choice
         * combinator, `+` just serves to hold the two strategies that are
         * chosen between by the conditional choice.
         */
        def + (q : => Strategy) : PlusStrategy =
            new PlusStrategy (p, q)

        /**
         * Conditional choice: `c < l + r`. Construct a strategy that first
         * applies this strategy (`c`). If `c` succeeds, the strategy applies
         * `l` to the resulting term, otherwise it applies `r` to the original
         * subject term.
         */
        def < (lr : => PlusStrategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => lr.lhs (t2)
                        case None      => lr.rhs (t1)
                    }
            }

    }

    /**
     * Helper class to contain commonality of choice in non-deterministic
     * choice operator and then-else part of a conditional choice. Only
     * returned by the non-deterministic choice operator.
     */
    class PlusStrategy (p : => Strategy, q : => Strategy) extends Strategy {
        val lhs = p
        val rhs = q
        def apply (t : Term) = (p <+ q) (t)
    }

    /**
     * Make a strategy from a function `f`. The function return value
     * determines whether the strategy succeeds or fails.
     */
    def strategyf (f : Term => Option[Term]) : Strategy =
        new Strategy {
            def apply (t : Term) = f (t)
        }

    /**
     * Make a strategy from a partial function `f`. If the function is
     * defined at the current term, then the function return value
     * when applied to the current term determines whether the strategy
     * succeeds or fails. If the function is not defined at the current
     * term, the strategy fails.
     */
    def strategy (f : Term ==> Option[Term]) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                if (f isDefinedAt t) {
                    f (t)
                } else {
                    None
                }
            }
        }

    /**
     * Define a rewrite rule using a function `f` that returns a term.
     * The rule always succeeds with the return value of the function.
     */
    def rulef (f : Term => Term) : Strategy =
        strategyf (t => Some (f (t)))

    /**
     * Define a rewrite rule using a partial function `f`. If the function is
     * defined at the current term, then the strategy succeeds with the return
     * value of the function applied to the current term. Otherwise the
     * strategy fails.
     */
    def rule (f : Term ==> Term) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                if (f isDefinedAt t) {
                    Some (f (t))
                } else {
                    None
                }
            }
        }

    /**
     * Define a rewrite rule using a function `f` that returns a strategy.  The
     * rule applies the function to the subject term to get a strategy which
     * is then applied again to the subject term. In other words, the function
     * is only used for side-effects such as pattern matching.  The whole thing
     * also fails if `f` is not defined at the term in the first place.
     */
    def rulefs (f : Term ==> Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                if (f isDefinedAt t) {
                    (f (t)) (t)
                } else {
                    None
                }
            }
        }

    /**
     * Construct a strategy that always succeeds, changing the subject term to
     * the given term `t`.
     */
    def build (t : => Term) : Strategy =
        rulef (_ => t)

    /**
     * Construct a strategy from an option value `o`. The strategy succeeds
     * or fails depending on whether `o` is a Some or None, respectively.
     * If `o` is a `Some`, then the subject term is changed to the term that
     * is wrapped by the `Some`.
     */
    def option (o : => Option[Term]) : Strategy =
        strategyf (_ => o)

    /**
     * Define a term query by a function `f`. The query always succeeds with
     * no effect on the subject term but applies the given (possibly partial)
     * function `f` to the subject term.  In other words, the strategy runs
     * `f` for its side-effects.
     */
    def queryf[T] (f : Term => T) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                f (t)
                Some (t)
            }
        }

    /**
     * Define a term query by a partial function `f`. The query always succeeds
     * with no effect on the subject term but applies the given partial function
     * `f` to the subject term.  In other words, the strategy runs `f` for its
     * side-effects.
     */
    def query[T] (f : Term ==> T) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                if (f isDefinedAt t) {
                    f (t)
                }
                Some (t)
            }
        }

    /**
     * A strategy that always fails.
     */
    val fail : Strategy =
        option (None)

    /**
     * A strategy that always succeeds with the subject term unchanged (i.e.,
     * this is the identity strategy).
     */
    val id : Strategy =
        strategyf (t => Some (t))

    /**
     * A strategy that always succeeds with the subject term unchanged (i.e.,
     * this is the identity strategy) with the side-effect that the subject
     * term is printed to the given emitter, prefixed by the string `s`.  The
     * emitter defaults to one that writes to standard output.
     */
    def debug (msg : String, emitter : Emitter = new Emitter) : Strategy =
        strategyf (t => { emitter.emitln (msg + t); Some (t) })

    /**
     * Create a logging strategy based on a strategy `s`. The returned strategy
     * succeeds or fails exactly as `s` does, but also prints the provided message,
     * the subject term, the success or failure status, and on success, the result
     * term, to the provided emitter (default: standard output).
     */
    def log[T] (s : => Strategy, msg : String, emitter : Emitter = new Emitter) : Strategy = 
        new Strategy {
            def apply (t1 : Term) = {
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
     * standard output).
     */
    def logfail[T] (s : => Strategy, msg : String, emitter : Emitter = new Emitter) : Strategy = 
        new Strategy {
            def apply (t1 : Term) = {
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
     * Construct a strategy that succeeds only if the subject term matches
     * the given term `t`.
     */
    def term (t : Term) : Strategy =
        rule {
            case `t` => t
        }

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
     * Cache of constructors for product duplication.
     */
    private val constrcache =
        new WeakHashMap[java.lang.Class[_], java.lang.reflect.Constructor[_]]

    /**
     * General product duplication function.  Returns a product that applies
     * the same constructor as the product `t`, but with the given children
     * instead of `t`'s children.  Fails if a constructor cannot be found or
     * if one of the children is not of the appropriate type.
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
    private def makechild (c : Any) : AnyRef =
        c.asInstanceOf[AnyRef]

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
     */
    def child (i : Int, s : Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] =
                t match {
                    case p : Product => childProduct (p)
                    case t : Seq[_]  => childSeq (t.asInstanceOf[Seq[Term]])
                    case _           => None
                }
                
            private def childProduct (p : Product) : Option[Term] = {
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
            
            private def childSeq[CC[U] <: Seq[U]] (t : CC[Term])
                            (implicit cbf : CanBuildFrom[CC[Term], Term, CC[Term]])
                                : Option[CC[Term]] = {
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
        }

    /**
     * Compare two arbitrary values. If they are both references, use 
     * reference equality, otherwise throw an error since we should be
     * able to cast anything to reference.
     */
    def same (v1 : Any, v2 : Any) : Boolean =
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
     */
    def all (s : => Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] =
                t match {
                    case r : Rewritable     => allRewritable (r)
                    case p : Product        => allProduct (p)
                    case m : Map[_,_]       => allMap (m.asInstanceOf[Map[Term,Term]])
                    case t : Traversable[_] => allTraversable (t.asInstanceOf[Traversable[Term]])
                    case _                  => Some (t)
                }

            private def allProduct (p : Product) : Option[Term] = {
                val numchildren = p.productArity
                if (numchildren == 0) {
                    Some (p)
                } else {
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
                
            private def allRewritable (r : Rewritable) : Option[Term] = {
                val numchildren = r.arity
                if (numchildren == 0) {
                    Some (r)
                } else {
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

            private def allTraversable[CC[_] <: Traversable[Term]] (t : CC[Term])
                            (implicit cbf : CanBuildFrom[CC[Term], Term, CC[Term]])
                                : Option[CC[Term]] =
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

            private def allMap[CC[V,W] <: Map[V,W]] (t : CC[Term,Term])
                            (implicit cbf : CanBuildFrom[CC[Term,Term], (Term, Term), CC[Term,Term]])
                                : Option[CC[Term,Term]] =
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
     */
    def one (s : => Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] =
                t match {
                    case r : Rewritable     => oneRewritable (r)
                    case p : Product        => oneProduct (p)
                    case m : Map[_,_]       => oneMap (m.asInstanceOf[Map[Term,Term]])
                    case t : Traversable[_] => oneTraversable (t.asInstanceOf[Traversable[Term]])
                    case _                  => None
                }

            private def oneProduct (p : Product) : Option[Term] = {
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

            private def oneRewritable (r : Rewritable) : Option[Term] = {
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
            
            private def oneTraversable [CC[U] <: Traversable[U]] (t : CC[Term])
                            (implicit cbf : CanBuildFrom[CC[Term], Term, CC[Term]])
                                : Option[CC[Term]] = {
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

            private def oneMap[CC[V,W] <: Map[V,W]] (t : CC[Term,Term])
                            (implicit cbf : CanBuildFrom[CC[Term,Term], (Term, Term), CC[Term,Term]])
                                : Option[CC[Term,Term]] = {
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
     */
    def some (s : => Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] =
                t match {
                    case r : Rewritable     => someRewritable (r)
                    case p : Product        => someProduct (p)
                    case m : Map[_,_]       => someMap (m.asInstanceOf[Map[Term,Term]])
                    case t : Traversable[_] => someTraversable (t.asInstanceOf[Traversable[Term]])
                    case _                  => None
                }

            private def someProduct (p : Product) : Option[Term] = {
                val numchildren = p.productArity
                if (numchildren == 0) {
                    None
                } else {
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
                        } else {
                            Some (p)
                        }
                    else
                        None
                }
            }

            private def someRewritable (r : Rewritable) : Option[Term] = {
                val numchildren = r.arity
                if (numchildren == 0) {
                    None
                } else {
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
                        } else {
                            Some (r)
                        }
                    else
                        None
                }
            }

            private def someTraversable[CC[U] <: Traversable[U]] (t : CC[Term])
                            (implicit cbf : CanBuildFrom[CC[Term], Term, CC[Term]])
                                : Option[CC[Term]] =
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

            private def someMap[CC[V,W] <: Map[V,W]] (t : CC[Term,Term])
                            (implicit cbf : CanBuildFrom[CC[Term,Term], (Term, Term), CC[Term,Term]])
                                : Option[CC[Term,Term]] =
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
        new Strategy {
            def apply (t : Term) : Option[Term] =
                t match {
                    case p : Product        => congruenceProduct (p, ss : _*)
                    case _                  => Some (t)
                }
                
            private def congruenceProduct (p : Product, ss : Strategy*) : Option[Term] = {
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
        }

    /**
     * Rewrite a term.  Apply the strategy `s` to a term returning the result term
     * if `s` succeeds, otherwise return the original term.
     */
    def rewrite[T] (s : => Strategy) (t : T) : T = {
        s (t) match {
            case Some (t1) =>
                t1.asInstanceOf[T]
            case None =>
                t
        }
    }

    /**
     * Return a strategy that behaves as `s` does, but memoises its arguments and
     * results.  In other words, if `memo(s)` is called on a term `t` twice, the
     * second time will return the same result as the first, without having to
     * invoke `s`.  For best results, it is important that `s` should have no side
     * effects.
     */
    def memo (s : => Strategy) : Strategy =
        new Strategy {
            import scala.collection.mutable.HashMap
            private val cache = new HashMap[Term,Option[Term]]
            def apply (t : Term) : Option[Term] =
                cache.getOrElseUpdate (t, s (t))
        }

    /**
     * Collect query results in a traversable collection.  Run the function
     * `f` as a top-down left-to-right query on the subject term.  Accumulate
     * the values produced by the function in the collection and return the
     * final value of the list.
     */
    def collect[CC[U] <: Traversable[U],T] (f : Term ==> T)
            (implicit cbf : CanBuildFrom[CC[T],T,CC[T]]) : Term => CC[T] =
        (t : Term) => {
            val b = cbf ()
            val add = (v : T) => b += v
            (everywhere (query (f andThen add))) (t)
            b.result ()
        }
        
    /**
     * Collect query results in a list.  Run the function `f` as a top-down
     * left-to-right query on the subject term.  Accumulate the values
     * produced by the function in a list and return the final value of
     * the list.
     */
    def collectl[T] (f : Term ==> T) : Term => List[T] =
        collect[List,T] (f)

    /**
     * Collect query results in a set.  Run the function `f` as a top-down
     * left-to-right query on the subject term.  Accumulate the values
     * produced by the function in a set and return the final value of
     * the set.
     */
    def collects[T] (f : Term ==> T) : Term => Set[T] =
        collect[Set,T] (f)

    /**
     * Count function results.  Run the function `f` as a top-down query on
     * the subject term.  Sum the integer values returned by `f` from all
     * applications.
     */
    def count (f : Term ==> Int) : Term => Int =
        everything (0) (_ + _) (f)

    /**
     * Construct a strategy that applies `s` to each element of a list,
     * returning a new list of the results if all of the applications
     * succeed, otherwise fail.  If all of the applications succeed
     * without change, return the input list.
     */
    def map (s : => Strategy) : Strategy =
        rulefs {
            case Nil           => id
            case l @ (x :: xs) =>
                option (s (x)) <*
                    rulefs {
                        case y =>
                            option (map (s) (xs)) <* 
                                rule {
                                    case ys : List[_] =>
                                        if (same (x, y) && same (xs, ys))
                                            l
                                        else
                                            y :: ys
                                }
                    }
        }

    /**
     * Construct a strategy that applies `s`, yielding the result of `s` if it
     * succeeds, otherwise leave the original subject term unchanged.  In
     * Stratego library this strategy is called `try`.
     */
    def attempt (s : => Strategy) : Strategy =
        s <+ id

    /**
     * Construct a strategy that applies `s` repeatedly until it fails.
     */
    def repeat (s : => Strategy) : Strategy =
        attempt (s <* repeat (s))

    /**
     * Construct a strategy that repeatedly applies `s` until it fails and
     * then terminates with application of `c`.
     */
    def repeat (s : => Strategy, c : => Strategy) : Strategy =
        (s <* repeat (s, c)) <+ c

    /**
     * Construct a strategy that applies `s` repeatedly exactly `n` times. If
     * `s` fails at some point during the n applications, the entire strategy
     * fails. The result of the strategy is that of the ''nth'' application of
     * `s`.
     */
    def repeat (s : => Strategy, n : Int) : Strategy =
        if (n == 0) id else s <* repeat (s, n - 1)

    /**
     * Construct a strategy that repeatedly applies `s` (at least once) and
     * terminates with application of `c`.
     */
    def repeat1 (s : => Strategy, c : => Strategy) : Strategy =
        s <* (repeat1 (s, c) <+ c)

    /**
     * Construct a strategy that repeatedly applies `s` (at least once).
     */
    def repeat1 (s : => Strategy) : Strategy =
        repeat1 (s, id)

    /**
     * Construct a strategy that repeatedly applies `s` until `c` succeeds.
     */
    def repeatuntil (s : => Strategy, c : => Strategy) : Strategy =
        s <* (c <+ repeatuntil (s, c))

    /**
     * Construct a strategy that while c succeeds applies `s`.  This operator
     * is called `while` in the Stratego library.
     */
    def loop (c : => Strategy, s : => Strategy) : Strategy =
        attempt (c <* s <* loop (c, s))

    /**
     * Construct a strategy that while `c` does not succeed applies `s`.  This
     * operator is called `while-not` in the Stratego library.
     */
    def loopnot (c : => Strategy, s : => Strategy) : Strategy =
        c <+ (s <* loopnot (c, s))

    /**
     * Construct a strategy that applies `s` at least once and then repeats `s`
     * while `c` succeeds.  This operator is called `do-while` in the Stratego
     * library.
     */
    def doloop (s : => Strategy, c : => Strategy) : Strategy =
       s <* loop (c, s)

    /**
     * Construct a strategy that repeats application of `s` while `c` fails, after
     * initialization with `i`.  This operator is called `for` in the Stratego
     * library.
     */
    def loopiter (i : => Strategy, c : => Strategy, s : => Strategy) : Strategy =
        i <* loopnot (c, s)

    /**
     * Construct a strategy that applies `s(i)` for each integer `i` from `low` to
     * `high` (inclusive).  This operator is called `for` in the Stratego library.
     */
    def loopiter (s : Int => Strategy, low : Int, high : Int) : Strategy =
        if (low <= high)
            s (low) <* loopiter (s, low + 1, high)
        else
            id

    /**
     * Construct a strategy that applies `s`, then fails if `s` succeeded or, if `s`
     * failed, succeeds with the subject term unchanged,  I.e., it tests if
     * `s` applies, but has no effect on the subject term.
     */
    def not (s : => Strategy) : Strategy =
        s < fail + id

    /**
     * Construct a strategy that tests whether strategy `s` succeeds,
     * restoring the original term on success.  This is similar
     * to Stratego's `where`, except that in this version any effects on
     * bindings are not visible outside `s`.
     */
    def where (s : => Strategy) : Strategy =
        strategyf (t => (s <* build (t)) (t))

    /**
     * Construct a strategy that tests whether strategy `s` succeeds,
     * restoring the original term on success.  A synonym for `where`.
     */
    def test (s : => Strategy) : Strategy =
        where (s)

    /**
     * Construct a strategy that applies `s` in breadth first order.
     */
    def breadthfirst (s : => Strategy) : Strategy =
        all (s) <* all (breadthfirst (s))

    /**
     * Construct a strategy that applies `s` in a top-down, prefix fashion
     * to the subject term.
     */
    def topdown (s : => Strategy) : Strategy =
        s <* all (topdown (s))

    /**
     * Construct a strategy that applies `s` in a top-down, prefix fashion
     * to the subject term but stops when the strategy produced by `stop`
     * succeeds. `stop` is given the whole strategy itself as its argument.
     */
    def topdownS (s : => Strategy, stop : (=> Strategy) => Strategy) : Strategy =
        s <* (stop (topdownS (s, stop)) <+ all (topdownS (s, stop)))

    /**
     * Construct a strategy that applies `s` in a bottom-up, postfix fashion
     * to the subject term.
     */
    def bottomup (s : => Strategy) : Strategy =
        all (bottomup (s)) <* s

    /**
     * Construct a strategy that applies `s` in a bottom-up, postfix fashion
     * to the subject term but stops when the strategy produced by `stop`
     * succeeds. `stop` is given the whole strategy itself as its argument.
     */
    def bottomupS (s : => Strategy, stop : (=> Strategy) => Strategy) : Strategy =
        (stop (bottomupS (s, stop)) <+ (all (bottomupS (s, stop))) <* s)

    /**
     * Construct a strategy that applies `s` in a combined top-down and
     * bottom-up fashion (i.e., both prefix and postfix) to the subject
     * term.
     */
    def downup (s : => Strategy) : Strategy =
        s <* all (downup (s)) <* s

    /**
     * Construct a strategy that applies `s1` in a top-down, prefix fashion
     * and `s2` in a bottom-up, postfix fashion to the subject term.
     */
    def downup (s1 : => Strategy, s2 : => Strategy) : Strategy =
        s1 <* all (downup (s1, s2)) <* s2

    /**
     * Construct a strategy that applies `s` in a combined top-down and
     * bottom-up fashion (i.e., both prefix and postfix) to the subject
     * but stops when the strategy produced by `stop` succeeds. `stop` is
     * given the whole strategy itself as its argument.
     */
    def downupS (s : => Strategy, stop : (=> Strategy) => Strategy) : Strategy =
        s <* (stop (downupS (s, stop)) <+ all (downupS (s, stop))) <* s

    /**
     * Construct a strategy that applies `s1` in a top-down, prefix fashion
     * and `s2` in a bottom-up, postfix fashion to the subject term but stops
     * when the strategy produced by `stop` succeeds. `stop` is given the whole
     * strategy itself as its argument.
     */
    def downupS (s1 : => Strategy, s2 : => Strategy, stop : (=> Strategy) => Strategy) : Strategy =
        s1 <* (stop (downupS (s1, s2, stop)) <+ all (downupS (s1, s2, stop))) <* s2

    /**
     * A unit for `topdownS`, `bottomupS` and `downupS`.  For example, `topdown(s)`
     * is equivalent to `topdownS(s, dontstop)`.
     */
    def dontstop (s : => Strategy) : Strategy =
        fail

    /**
     * Construct a strategy that applies `s` in a top-down fashion to one
     * subterm at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def oncetd (s : => Strategy) : Strategy =
        s <+ one (oncetd (s))

    /**
     * Construct a strategy that applies `s` in a bottom-up fashion to one
     * subterm at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def oncebu (s : => Strategy) : Strategy =
        one (oncebu (s)) <+ s

    /**
     * Construct a strategy that applies `s` in a top-down fashion to some
     * subterms at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def sometd (s : => Strategy) : Strategy =
        s <+ some (sometd (s))

    /**
     * Construct a strategy that applies `s` in a bottom-up fashion to some
     * subterms at each level, stopping as soon as it succeeds once (at
     * any level).
     */
    def somebu (s : => Strategy) : Strategy =
        some (somebu (s)) <+ s

    /**
     * Construct a strategy that applies `s` repeatedly in a top-down fashion
     * stopping each time as soon as it succeeds once (at any level). The
     * outermost fails when `s` fails to apply to any (sub-)term.
     */
    def outermost (s : => Strategy) : Strategy =
        repeat (oncetd (s))

    /**
     * Construct a strategy that applies `s` repeatedly to the innermost
     * (i.e., lowest and left-most) (sub-)term to which it applies.
     * Stop with the current term if `s` doesn't apply anywhere.
     */
    def innermost (s : => Strategy) : Strategy =
        bottomup (attempt (s <* innermost (s)))

    /**
     * An alternative version of `innermost`.
     */
    def innermost2 (s : => Strategy) : Strategy =
        repeat (oncebu (s))

    /**
     * Construct a strategy that applies `s` repeatedly to subterms
     * until it fails on all of them.
     */
    def reduce (s : => Strategy) : Strategy = {
        def x : Strategy = some (x) + s
        repeat (x)
    }

    /**
     * Construct a strategy that applies `s` in a top-down fashion, stopping
     * at a frontier where s succeeds.
     */
    def alltd (s : => Strategy) : Strategy =
        s <+ all (alltd (s))

    /**
     * Construct a strategy that applies `s` in a bottom-up fashion to all
     * subterms at each level, stopping at a frontier where s succeeds.
     */
    def allbu (s : => Strategy) : Strategy =
        all (allbu (s)) <+ s

    /**
     * Construct a strategy that applies `s1` in a top-down, prefix fashion
     * stopping at a frontier where `s1` succeeds.  `s2` is applied in a bottom-up,
     * postfix fashion to the result.
     */
    def alldownup2 (s1 : => Strategy, s2 : => Strategy) : Strategy =
        (s1 <+ all (alldownup2 (s1, s2))) <* s2

    /**
     * Construct a strategy that applies `s1` in a top-down, prefix fashion
     * stopping at a frontier where `s1` succeeds.  `s2` is applied in a bottom-up,
     * postfix fashion to the results of the recursive calls.
     */
    def alltdfold (s1 : => Strategy, s2 : => Strategy) : Strategy =
        s1 <+ (all (alltdfold (s1, s2)) <* s2)

   /**
    * Construct a strategy that applies `s` in a top-down, prefix fashion
    * stopping at a frontier where `s` succeeds on some children.  `s` is then
    * applied in a bottom-up, postfix fashion to the result.
    */
   def somedownup (s : => Strategy) : Strategy =
       (s <* all (somedownup (s)) <* (attempt (s))) <+ (some (somedownup (s)) <+ attempt (s))

    /**
     * Construct a strategy that applies `s` as many times as possible, but
     * at least once, in bottom up order.
     */
    def manybu (s : Strategy) : Strategy =
        some (manybu (s)) <* attempt (s) <+ s

    /**
     * Construct a strategy that applies `s` as many times as possible, but
     * at least once, in top down order.
     */
    def manytd (s : Strategy) : Strategy =
        s <* all (attempt (manytd (s))) <+ some (manytd (s))

    /**
     * Construct a strategy that tests whether the two sub-terms of a
     * pair of terms are equal.
     */
    val eq : Strategy =
       rule {
           case t @ (x, y) if x == y => t
       }

    /**
     * Construct a strategy that tests whether the two sub-terms of a
     * pair of terms are equal. Synonym for `eq`.
     */
    val equal : Strategy =
        eq

    /**
     * Construct a strategy that succeeds when applied to a pair `(x,y)`
     * if `x` is a sub-term of `y`.
     */
    val issubterm : Strategy =
        strategy {
            case (x : Term, y : Term) => where (oncetd (term (x))) (y)
        }

    /**
     * Construct a strategy that succeeds when applied to a pair `(x,y)`
     * if `x` is a sub-term of `y` but is not equal to `y`.
     */
    val ispropersubterm : Strategy =
        not (eq) <* issubterm

    /**
     * Construct a strategy that succeeds when applied to a pair `(x,y)`
     * if `x` is a superterm of `y`.
     */
    val issuperterm : Strategy =
        strategy {
            case (x, y) => issubterm (y, x)
        }

    /**
     * Construct a strategy that succeeds when applied to a pair `(x,y)`
     * if `x` is a super-term of `y` but is not equal to `y`.
     */
    val ispropersuperterm : Strategy =
        not (eq) <* issuperterm

    /**
     * Construct a strategy that succeeds if the current term has no
     * direct subterms.
     */
    val isleaf : Strategy =
      all (fail)

    /**
     * Construct a strategy that applies to all of the leaves of the
     * current term, using `isleaf` as the leaf predicate.
     */
    def leaves (s : => Strategy, isleaf : => Strategy) : Strategy =
        (isleaf <* s) <+ all (leaves (s, isleaf))

    /**
     * Construct a strategy that applies to all of the leaves of the
     * current term, using `isleaf` as the leaf predicate, skipping
     * subterms for which `skip` when applied to the result succeeds.
     */
    def leaves (s : => Strategy, isleaf : => Strategy, skip : Strategy => Strategy) : Strategy =
        (isleaf <* s) <+ skip (leaves (s, isleaf, skip)) <+ all (leaves (s, isleaf, skip))

    /**
     * Construct a strategy that succeeds if the current term has at
     * least one direct subterm.
     */
    val isinnernode : Strategy =
        one (id)

    /**
     * Construct a strategy that applies `s` at all terms in a bottom-up fashion
     * regardless of failure.  Terms for which the strategy fails are left
     * unchanged.
     */
    def everywherebu (s : => Strategy) : Strategy =
        bottomup (attempt (s))

    /**
     * Construct a strategy that applies `s` at all terms in a top-down fashion
     * regardless of failure.  Terms for which the strategy fails are left
     * unchanged.
     */
    def everywheretd (s : => Strategy) : Strategy =
        topdown (attempt (s))

    /**
     * Same as `everywheretd`.
     */
    def everywhere (s : => Strategy) : Strategy =
        everywheretd (s)

    /**
     * Apply the function at every term in `t` in a top-down, left-to-right order.
     * Collect the resulting `T` values by accumulating them using `f` with initial
     * left value `v`.  Return the final value of the accumulation.
     */
    def everything[T] (v : T) (f : (T, T) => T) (g : Term ==> T) (t : Term) : T =
        (collectl (g) (t)).foldLeft (v) (f)

    /**
     * Construct a strategy that applies `s`, then applies the restoring action
     * `rest` if `s` fails (and then fail). Otherwise, let the result of `s` stand.
     * Typically useful if `s` performs side effects that should be restored or
     * undone when `s` fails.
     */
    def restore (s : => Strategy, rest : => Strategy) : Strategy =
        s <+ (rest <* fail)

    /**
     * Construct a strategy that applies `s`, then applies the restoring action
     * `rest` regardless of the success or failure of `s`. The whole strategy
     * preserves the success or failure of `s`. Typically useful if `s` performs
     * side effects that should be restored always, e.g., when maintaining scope
     * information.
     */
    def restorealways (s : => Strategy, rest : => Strategy) : Strategy =
        s < rest + (rest <* fail)

    /**
     * Applies `s` followed by `f` whether `s` failed or not.
     * This operator is called `finally` in the Stratego library.
     */
    def lastly (s : => Strategy, f : => Strategy) : Strategy =
        s < where (f) + (where (f) <* fail)

    /**
     * `ior(s1, s2)` implements inclusive OR, that is, the
     * inclusive choice of `s1` and `s2`. It first tries `s1`. If
     * that fails it applies `s2` (just like `s1 <+ s2`). However,
     * when `s1` succeeds it also tries to apply `s2`.
     */
    def ior (s1 : => Strategy, s2 : => Strategy) : Strategy =
        (s1 <* attempt (s2)) <+ s2

    /**
     * `or(s1, s2)` is similar to `ior(s1, s2)`, but the application
     * of the strategies is only tested.
     */
    def or (s1 : => Strategy, s2 : => Strategy) : Strategy =
        where (s1) < attempt (test (s2)) + test (s2)

    /**
     * `and(s1, s2)` applies `s1` and `s2` to the current
     * term and succeeds if both succeed. `s2` will always
     * be applied, i.e., and is ''not'' a short-circuit
     * operator
     */
    def and (s1 : => Strategy, s2 : => Strategy) : Strategy =
        where (s1) < test (s2) + (test (s2) <* fail)

}

/**
 * Strategy-based term rewriting for arbitrary terms.
 */
object Rewriter extends Rewriter
