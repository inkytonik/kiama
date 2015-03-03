/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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
trait Rewriter extends RewriterCore {

    import org.kiama.relation.Tree
    import org.kiama.relation.Tree.isLeaf
    import scala.collection.generic.CanBuildFrom
    import scala.language.higherKinds

    /**
     * Rewrite a term.  Apply the strategy `s` to a term returning the result term
     * if `s` succeeds, otherwise return the original term.
     */
    def rewrite[T] (s : Strategy) (t : T) : T = {
        s (t) match {
            case Some (t1) =>
                t1.asInstanceOf[T]
            case None =>
                t
        }
    }

    /**
     * Rewrite a tree.  Apply the strategy `s` to the root of a tree returning the
     * a tree formed from the result term if `s` succeeds, otherwise return the
     * original tree.
     */
    def rewriteTree[T <: Product,U <: T] (s : Strategy) (t : Tree[T,U]) : Tree[T,U] = {
        s (t.root) match {
            case Some (t1) =>
                new Tree[T,U] (t1.asInstanceOf[U])
            case None =>
                t
        }
    }

    // Library combinators

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.allbu
     */
    def allbu (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = all (result) <+ s
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.alltd
     */
    def alltd (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = s <+ all (result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.alldownup2
     */
    def alldownup2 (name : String, s1 : Strategy, s2 : Strategy) : Strategy = {
        lazy val result : Strategy = (s1 <+ all (result)) <* s2
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.alltdfold
     */
    def alltdfold (name : String, s1 : Strategy, s2 : Strategy) : Strategy = {
        lazy val result : Strategy = s1 <+ (all (result) <* s2)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.and
     */
    def and (name : String, s1 : Strategy, s2 : Strategy) : Strategy =
        where (s1) < (name, test (s2) + (test (s2) <* fail))

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.attempt
     */
    def attempt (name : String, s : Strategy) : Strategy =
        s <+ (name, id)

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.bottomup
     */
    def bottomup (name : String, s : Strategy) : Strategy =
        all (bottomup (s)) <* (name, s)

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.bottomupS
     */
    def bottomupS (name : String, s : Strategy, stop : (=> Strategy) => Strategy) : Strategy = {
        lazy val result : Strategy = (stop (result) <+ all (result)) <* s
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.breadthfirst
     */
    def breadthfirst (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = all (s) <* all (result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.doloop
     */
    def doloop (name : String, s : Strategy, r : Strategy) : Strategy =
        s <* (name, loop (r, s))

    /**
     * A unit for `topdownS`, `bottomupS` and `downupS`.  For example, `topdown(s)`
     * is equivalent to `topdownS(s, dontstop)`.
     */
    def dontstop (s : => Strategy) : Strategy =
        fail

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.downup
     */
    def downup (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = s <* all (result) <* s
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.downup
     */
    def downup (name : String, s1 : Strategy, s2 : Strategy) : Strategy = {
        lazy val result : Strategy = s1 <* all (result) <* s2
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.downupS
     */
    def downupS (name : String, s : Strategy, stop : (=> Strategy) => Strategy) : Strategy = {
        lazy val result : Strategy = s <* (stop (result) <+ all (result) <* s)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.downupS
     */
    def downupS (name : String, s1 : Strategy, s2 : Strategy, stop : (=> Strategy) => Strategy) : Strategy = {
        lazy val result : Strategy = s1 <* (stop (result) <+ all (result) <* s2)
        mkStrategy (name, result)
    }

    /**
     * A strategy that tests whether the two sub-terms of a pair of terms are equal.
     */
    val eq : Strategy =
       ruleWithName[(Any,Any)] ("eq", {
           case t @ (x, y) if x == y => t
       })

    /**
     * Construct a strategy that tests whether the two sub-terms of a
     * pair of terms are equal. Synonym for `eq`.
     */
    val equal : Strategy =
        eq

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.everywhere
     */
    def everywhere (name : String, s : Strategy) : Strategy =
        everywheretd (name, s)

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.everywherebu
     */
    def everywherebu (name : String, s : Strategy) : Strategy =
        bottomup (name, attempt (s))

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.everywheretd
     */
    def everywheretd (name : String, s : Strategy) : Strategy =
        topdown (name, attempt (s))

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.innermost
     */
    def innermost (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = bottomup (attempt (s <* result))
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.innermost2
     */
    def innermost2 (name : String, s : Strategy) : Strategy =
        repeat (name, oncebu (s))

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.ior
     */
    def ior (name : String, s1 : Strategy, s2 : Strategy) : Strategy =
        (s1 <* attempt (s2)) <+ (name, s2)

    /**
     * Construct a strategy that succeeds if the current term has at
     * least one direct subterm.
     */
    val isinnernode : Strategy =
        mkStrategy ("isinnernode", one (id))

    /**
     * Construct a strategy that succeeds if the current term has no
     * direct subterms.
     */
    val isleaf : Strategy =
        mkStrategy ("isleaf", all (fail))

    /**
     * Construct a strategy that succeeds when applied to a pair `(x,y)`
     * if `x` is a sub-term of `y` but is not equal to `y`.
     */
    val ispropersubterm : Strategy =
        mkStrategy ("ispropersubterm", not (eq) <* issubterm)

    /**
     * Construct a strategy that succeeds when applied to a pair `(x,y)`
     * if `x` is a sub-term of `y`.
     */
    val issubterm : Strategy =
        mkStrategy ("issubterm", {
            case (x, y) =>
                where (oncetd (term (x))) (y)
        })

    /**
     * Construct a strategy that succeeds when applied to a pair `(x,y)`
     * if `x` is a superterm of `y`.
     */
    val issuperterm : Strategy =
        mkStrategy ("issuperterm", {
            case (x, y) =>
                issubterm ((y, x))
        })

    /**
     * Construct a strategy that succeeds when applied to a pair `(x,y)`
     * if `x` is a super-term of `y` but is not equal to `y`.
     */
    val ispropersuperterm : Strategy =
        mkStrategy ("ispropersuperterm", not (eq) <* issuperterm)

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.lastly
     */
    def lastly (name : String, s : Strategy, f : Strategy) : Strategy =
        s < (name, where (f) + (where (f) <* fail))

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.leaves
     */
    def leaves (name : String, s : Strategy, isleaf : Strategy) : Strategy = {
        lazy val result : Strategy = (isleaf <* s) <+ all (result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.leaves
     */
    def leaves (name : String, s : Strategy, isleaf : Strategy, skip : Strategy => Strategy) : Strategy = {
        lazy val result : Strategy = (isleaf <* s) <+ skip (result) <+ all (result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.loop
     */
    def loop (name : String, c : Strategy, s : Strategy) : Strategy = {
        lazy val result : Strategy = attempt (c <* s <* result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.loopiter
     */
    def loopiter (name : String, i : Strategy, r : Strategy, s : Strategy) : Strategy =
        i <* (name, loopnot (r, s))

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.loopiter
     */
    def loopiter (name : String, s : Int => Strategy, low : Int, high : Int) : Strategy = {
        lazy val result =
            if (low <= high)
                s (low) <* loopiter (s, low + 1, high)
            else
                id
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.loopnot
     */
    def loopnot (name : String, r : Strategy, s : Strategy) : Strategy = {
        lazy val result : Strategy = r <+ (s <* result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.map
     */
    def map (name : String, s : Strategy) : Strategy =
        strategyWithName[Seq[_]] (name, {
            case l : Seq[_] =>
                allTraversable[Seq] (s, l)
        })

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.manybu
     */
    def manybu (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = some (result) <* attempt (s) <+ s
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.manytd
     */
    def manytd (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = s <* all (attempt (result)) <+ some (result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.not
     */
    def not (name : String, s : Strategy) : Strategy =
        s < (name, fail + id)

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.oncebu
     */
    def oncebu (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = one (result) <+ s
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.oncetd
     */
    def oncetd (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = s <+ one (result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.or
     */
    def or (name : String, s1 : Strategy, s2 : Strategy) : Strategy =
        where (s1) < (name, attempt (test (s2)) + test (s2))

    /*
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.outermost
     */
    def outermost (name : String, s : Strategy) : Strategy =
        repeat (name, oncetd (s))

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.reduce
     */
    def reduce (name : String, s : Strategy) : Strategy = {
        lazy val inner : Strategy = some (inner) + s
        repeat (name, inner)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.repeat
     */
    def repeat (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = attempt (s <* result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.repeat
     */
    def repeat (name : String, s : Strategy, r : Strategy) : Strategy = {
        lazy val result : Strategy = (s <* result) <+ r
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.repeat
     */
    def repeat (name : String, s : Strategy, n : Int) : Strategy = {
        lazy val result = if (n == 0) id else s <* repeat (s, n - 1)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.repeat1
     */
    def repeat1 (name : String, s : Strategy) : Strategy =
        repeat1 (name, s, id)

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.repeat1
     */
    def repeat1 (name : String, s : Strategy, r : Strategy) : Strategy = {
        lazy val result : Strategy = s <* (result <+ r)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.repeatuntil
     */
    def repeatuntil (name : String, s : Strategy, r : Strategy) : Strategy = {
        lazy val result : Strategy = s <* (r <+ result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.restore
     */
    def restore (name : String, s : Strategy, rest : Strategy) : Strategy =
        s <+ (name, rest <* fail)

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.restorealways
     */
    def restorealways (name : String, s : Strategy, rest : Strategy) : Strategy =
        s < (name, rest + (rest <* fail))

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.somebu
     */
    def somebu (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = some (result) <+ s
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.somedownup
     */
    def somedownup (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = (s <* all (result) <* (attempt (s))) <+ some (result) <+ attempt (s)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.sometd
     */
    def sometd (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = s <+ some (result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.test
     */
    def test (name : String, s : Strategy) : Strategy =
        where (name, s)

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.topdown
     */
    def topdown (name : String, s : Strategy) : Strategy = {
        lazy val result : Strategy = s <* all (result)
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.topdownS
     */
    def topdownS (name : String, s : Strategy, stop : (=> Strategy) => Strategy) : Strategy = {
        lazy val result : Strategy = s <* (stop (result) <+ all (result))
        mkStrategy (name, result)
    }

    /**
     * As for the version in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.where
     */
    def where (name : String, s : Strategy) : Strategy =
        strategyf (name, t => (s <* build (t)) (t))

    // Queries below here

    /**
     * As for `collect` in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.collect
     */
    def collectWithName[CC[X] <: Traversable[X],U] (name : String, f : Any ==> U)
            (implicit cbf : CanBuildFrom[CC[Any],U,CC[U]]) : Any => CC[U] =
        (t : Any) => {
            val b = cbf ()
            def add (u : U) { b += u }
            (everywhere (query (f andThen add))) (t)
            b.result
        }

    /**
     * As for `collectall` in `RewriterCore` without the `name` argument but
     * specifies the name for the constructed strategy.
     * @see RewriterCore.collectall
     */
    def collectallWithName[CC[X] <: Traversable[X],U] (name : String, f : Any ==> CC[U])
            (implicit cbf : CanBuildFrom[CC[Any],U,CC[U]]) : Any => CC[U] =
        (t : Any) => {
            val b = cbf ()
            def addall (us : CC[U]) { b ++= us }
            (everywhere (query (f andThen addall))) (t)
            b.result
        }

    /**
     * Collect query results in a list.  Run the function `f` as a top-down
     * left-to-right query on the subject term.  Accumulate the values
     * produced by the function in a list and return the final value of
     * the list.
     */
    def collectl[U] (f : Any ==> U) : Any => List[U] =
        collect[List,U] (f)

    /**
     * Collect query results in a set.  Run the function `f` as a top-down
     * left-to-right query on the subject term.  Accumulate the values
     * produced by the function in a set and return the final value of
     * the set.
     */
    def collects[U] (f : Any ==> U) : Any => Set[U] =
        collect[Set,U] (f)

    /**
     * Count function results.  Run the function `f` as a top-down query on
     * the subject term.  Sum the integer values returned by `f` from all
     * applications.
     */
    def count (name : String, f : Any ==> Int) : Any => Int =
        everything (name, 0) (_ + _) (f)

    /**
     * Apply the function at every term in `t` in a top-down, left-to-right order.
     * Collect the resulting `T` values by accumulating them using `f` with initial
     * left value `v`.  Return the final value of the accumulation.
     */
    def everything[T] (name : String, v : T) (f : (T, T) => T) (g : Any ==> T) (t : Any) : T = {
        val collector = collectWithName[List,T] (name, g)
        collector (t).foldLeft (v) (f)
    }

    // Other higher-level operations

    /**
     * Deep clone the term `t`. Only applicable if the base type of the tree is
     * a `Product`.
     */
    def deepclone[T <: Product] (t : T) : T = {

        val deepcloner =
            everywherebu (rule[T] {
                case n if isLeaf (n) =>
                    copy (n)
            })

        rewrite (deepcloner) (t)

    }

}

/**
 * Strategy-based term rewriting for arbitrary terms.
 */
object Rewriter extends Rewriter
