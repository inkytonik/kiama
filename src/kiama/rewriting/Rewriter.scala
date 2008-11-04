/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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
                                
package kiama.rewriting

/**
 * Strategy-based term rewriting in the style of Stratego (http://strategoxt.org/).
 * The implementation here is partially based on the semantics given in "Program
 * Transformation with Scoped Dynamic Rewrite Rules", by Bravenboer, van Dam, Olmos
 * and Visser, Fundamenta Informaticae, 69, 2005. The library strategies are mostly
 * based on the Stratego library, but also on combinators found in the Scrap Your
 * Boilerplate and Uniplate libraries for Haskell.
 */
trait Rewriter {
    
    /**
     * Debug flag. Set this to true in sub-classes or objects to obtain
     * tracing information during rewriting.
     */
    def debug = false

    /**
     * The type of terms that can be rewritten.  Any type of object value is
     * acceptable but generic traversals will only work on Products (e.g.,
     * instances of case classes).
     */
    type Term = AnyRef

    /**
     * Term-rewriting strategies.
     */
    abstract class Strategy extends Function[Term,Option[Term]] {
        
        /**
         * Alias this strategy as p to make it easier to refer to in the
         * combinator definitions.
         */
        p =>
        
        /**
         * Apply this strategy to a term, producing either a transformed term
         * or None, representing a rewriting failure.
         */
        def apply (r : Term) : Option[Term]
        
        /**
         * Sequential composition.  Construct a strategy that first applies
         * this strategy. If it succeeds, then apply q to the new subject
         * term.  Otherwise fail.
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
         * this strategy.  If it succeeds, succeed with the resulting term.
         * Otherwise, apply q to the original subject term.
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
         * Non-deterministic choice.  Construct a strategy that first applies
         * either this strategy or the given strategy.  If it succeeds,
         * succeed with the resulting term. Otherwise, apply q.
         * Currently implemented as deterministic choice, but this behaviour
         * should not be relied upon.
         */
        def + (q : => Strategy) : Strategy =
            <+ (q)
            
        /**
         * Guarded choice.  Construct a strategy that first applies this 
         * strategy.  If it succeeds, apply l to the resulting term,
         * otherwise apply r to the original subject term.
         */
        def <++ (l : => Strategy, r: => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => l (t2)
                        case None      => r (t1)
                    }
            }

    }
    
    /**
     * A strategy that always fails.
     */
    val failure : Strategy =
        new Strategy {
            def apply (t : Term) = None
        }
    
    /**
     * A strategy that always succeeds with the subject term unchanged (i.e.,
     * this is the identity strategy).
     */
    val id : Strategy =
        new Strategy {
            def apply (t : Term) = Some (t)
        }
        
    /**
     * (Implicitly) construct a strategy that always succeeds, changing the subject
     * term to a given term.
     */
    implicit def termToStrategy (a : Term) =
        new Strategy {
            def apply (t : Term) = Some (a)
        }
    
    /**
     * Define a rewrite rule.  Construct a strategy based on a (possibly partial)
     * function f. If f does not apply, the strategy fails, otherwise the strategy
     * succeeds with the result of aplying f to the subject term.
     */
    def rule (f : PartialFunction[Term,Term]) : Strategy =
        new Strategy {
            def apply (t : Term) =
                if (f isDefinedAt t) {
                    if (debug) println ("rule success: " + t + " => " + f (t))
                    Some (f (t))
                } else {
                    if (debug) println ("rule failure: " + t)
                    None
                }
        }

    /**
     * Define a term query.  Construct a strategy that always succeeds with no
     * effect on the subject term but applies a given (possibly partial)
     * function f to the subject term.  In other words, the strategy runs f
     * for its side-effects.
     */
    def query[T] (f : PartialFunction[Term,T]) : Strategy =
        new Strategy {
            def apply (t : Term) = {
                if (f isDefinedAt t) {
                    val v = f (t)
                    if (debug) println ("query success: " + t + " => " + v)
                } else {
                    if (debug) println ("query failure: " + t)
                }
                Some (t)
            }
        }

    /**
     * Generic term deconstruction.
     */
    object Term {
      
        /**
         * Generic term deconstruction.  An extractor that decomposes Products
         * into the product itself and a sequence of its children.  Terms that
         * are not products are not decomposable (ie the list of children will
         * be empty).
         */
        def unapply (t : Any) : Option[(Any,Seq[Any])] = {
            t match {
                case p : Product => {
                    val cs = for (i <- 0 until p.productArity) yield p.productElement (i)
                    Some ((p, cs))
                }
                case _ =>
                    Some ((t, Nil))
            }
        }
    }
    
    /**
     * Perform a paramorphism over a value.  This is a fold in which the
     * recursive step may refer to the recursive component of the value
     * and the results of folding over the children.  When the function f
     * is called, the first parameter is the value and the second is a
     * sequence of the values that f has returned for the children.  This
     * will work on any value, but will only decompose Products.  This 
     * operation is similar to that used in the Uniplate library.
     */
    def para[T] (f : (Any, Seq[T]) => T) : Any => T = {
        case Term (t, ts) => f (t, ts.map (para (f)))
    }

    /**
     * General product duplication function.  Returns a product that applies
     * the same constructor as the product t, but with the given children
     * instead of t's children.  Fails if a constructor cannot be found or
     * if one of the children is not of the appropriate type.
     */
    private def dup (t : Product, children : Array[AnyRef]) : Product = {
        val ctor = (t.getClass.getConstructors())(0)
        try {
            ctor.newInstance (children : _*).asInstanceOf[Product]
        } catch {
            case e : java.lang.ClassCastException =>
                error ("dup cast failed: " + t)
            case e : IllegalArgumentException =>
                error ("dup illegal arguments: " + ctor + " " + children.deepMkString (",") +
                       " (expects " + ctor.getParameterTypes.length + ")")
        }
    }

    /**
     * Make an arbitrary value into a term child, checking that it worked properly.
     * Object references will be returned unchanged; other values will be boxed.
     */
    private def makechild (child : Any) : AnyRef = {
        try {
            return child.asInstanceOf[AnyRef]
        } catch {
            case e : ClassCastException =>
                error ("makechild: can't cast child: " + child + " " + e)
        }
    }        

    /**
     * Traversal to all children.  Construct a strategy that applies s to all
     * term children of the subject term in left-to-right order.  If s succeeds
     * on all of the children, then succeed, forming a new term from the constructor
     * of the original term and the result of s for each child.  If s fails on any
     * child, fail.
     */
    def all (s : => Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] = {
                if (debug)
                    println ("all: " + t)
                t match {
                    case p : Product =>
                        val numchildren = p.productArity
                        if (numchildren == 0) {
                            if (debug)
                                println ("all success (no children): " + p)
                            Some (t)
                        } else {
                            val children = new Array[AnyRef](numchildren)
                            for (i <- 0 until numchildren) {
                                p.productElement (i) match {
                                    case ct : Term =>
                                        s (ct) match {
                                            case Some (ti) =>
                                                children (i) = ti
                                            case None      =>
                                                if (debug)
                                                    println ("all failure")
                                                    return None
                                        }
                                    case ci =>
                                        // Child is not a term, don't try to transform it
                                        children (i) = makechild (ci)
                                }
                            }
                            val ret = dup (p, children)
                            if (debug)
                                println ("all success: " + ret)
                            Some (ret)
                        }
                    case a =>
                        if (debug)
                            println ("all success (any): " + a)
                        Some (a)
                }
            }
        }
    
    /**
     * Traversal to one child.  Construct a strategy that applies s to the term
     * children of the subject term in left-to-right order.  Assume that c is the
     * first child on which s succeeds.  Then stop applying s to the children and
     * succeed, forming a new term from the constructor of the original term and
     * the original children, except that c is replaced by the result of applying
     * s to c.  In the event that the strategy fails on all children, then fail.
     */
    def one (s : => Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] = {
                if (debug)
                    println ("one: " + t)
                t match {
                    case p : Product =>
                        val numchildren = p.productArity
                        for (i <- 0 until numchildren) {
                            p.productElement (i) match {
                                case ct : Term =>
                                    s (ct) match {
                                        case Some (nct) => {
                                            val children = new Array[AnyRef] (numchildren)
                                            for (j <- 0 until i)
                                                children (j) = makechild (p.productElement (j))
                                            children (i) = nct
                                            for (j <- i + 1 until numchildren)
                                                children (j) = makechild (p.productElement (j))
                                            val ret = dup (p, children)
                                            if (debug)
                                                println ("one success: " + ret)
                                            return Some (ret)
                                        }
                                        case None => 
                                            // Transformation failed, this child stays unchanged
                                    }
                                case _ =>
                                    // Child is not a term, don't try to transform it
                            }
                        }
                        if (debug)
                            println ("one failure")
                        None
                    case _ =>
                        if (debug)
                            println ("one failure (any)")
                        None
                }
            }
        }
            
    /**
     * Rewrite a term.  Apply the strategy s to a term returning the result term
     * if s succeeds, otherwise return the original term.
     */
    def rewrite[T <: Term] (s : => Strategy) (t : T) : T = {
        if (debug)
            println ("rewrite: " + t)
        s (t) match {
            case Some (t1) => 
                if (debug)
                    println ("rewrite success: " + t + " => " + t1)
                t1.asInstanceOf[T]
            case None =>
                if (debug)
                    println ("rewrite failure: " + t)
                t
        }
    }

    /**
     * Collect query results in a set.  Run the function f as a top-down
     * query on the subject term.  Accumulate the values produced by the
     * function in a set and return the final value of the set.
     */
    def collects[T] (f : PartialFunction[Term,T]) : Term => Set[T] =
        (t : Term) => {
            var collection = Set[T]()
            def collect = (v : T) => collection += v
            (everywheretd (query (f andThen collect))) (t)
            collection
        }
        
    /**
     * Collect query results in a list.  Run the function f as a top-down
     * query on the subject term.  Accumulate the values produced by the
     * function in a list and return the final value of the list.
     */
    def collectl[T] (f : PartialFunction[Term,T]) : Term => List[T] =
        (t : Term) => {
            var collection = List[T]()
            def collect = (v : T) => collection = collection ::: List (v)
            (everywheretd (query (f andThen collect))) (t)
            collection
        }
        
    /**
     * Count function results.  Run the function f as a top-down query on
     * the subject term.  Sum the integer values returned by f from all
     * applications.
     */
    def count (f : PartialFunction[Term,Int]) : Term => Int =
        (t : Term) => {
            var total = 0
            def count = (v : Int) => total += v
            (everywheretd (query (f andThen count))) (t)
            total
        }
    
    /**
     * Construct a strategy that applies s, yielding the result of s if it
     * succeeds, otherwise leave the original subject term unchanged.  In
     * Stratego library this strategy is called "try".
     */
    def attempt (s : => Strategy) : Strategy =
        s <+ id
          
    /**
     * Construct a strategy that applies s repeatedly until it fails.
     */
    def repeat (s : => Strategy) : Strategy =
        attempt (s <* repeat (s))
    
    /**
     * Construct a strategy that applies s, then fails if s succeeded or, if s
     * failed, succeeds with the subject term unchanged,  I.e., it tests if
     * s applies, but has no effect on the subject term.
     */
    def not (s : => Strategy) : Strategy =
        s <++ (failure, id)
        
    /**
     * Construct a strategy that applies s for its side-effects and always
     * succeed with the subject term unchanged.  This strategy is similar 
     * to Stratego's "where", except that in this version any effects on
     * bindings are not visible outside s. 
     */
    def where (s : => Strategy) : Strategy =
        rule { case t => s (t); t }

    /**
     * Construct a strategy that applies s in a top-down, prefix fashion
     * to the subject term.
     */
    def topdown (s : => Strategy) : Strategy =
        s <* all (topdown (s)) 

    /**
     * Construct a strategy that applies s in a bottom-up, postfix fashion
     * to the subject term.
     */
    def bottomup (s : => Strategy) : Strategy =
        all (bottomup (s)) <* s
  
    /**
     * Construct a strategy that applies s in a combined top-down and
     * bottom-up fashion (i.e., both prefix and postfix) to the subject
     * term.
     */
    def downup (s : => Strategy) : Strategy =
        s <* all (downup (s)) <* s 

    /**
     * Construct a strategy that applies s1 in a top-down, prefix fashion
     * and s2 in a bottom-up, postfix fashion to the subject term.
     */
    def downup (s1 : => Strategy, s2 : => Strategy) : Strategy =
        s1 <* all (downup (s1, s2)) <* s2

    /**
     * Construct a strategy that applies s in a top-down fashion stopping
     * as soon as it succeeds once (at any level).
     */
    def oncetd (s : => Strategy) : Strategy =
        s <+ one (oncetd (s))
    
    /**
     * Construct a strategy that applies s in a bottom-up fasion stopping
     * as soon as it succeeds once (at any level).
     */
    def oncebu (s : => Strategy) : Strategy =
        one (oncebu (s)) <+ s 

    /**
     * Construct a strategy that applies s repeatedly in a top-down fashion
     * stopping each time as soon as it succeeds once (at any level). The
     * outermost fails when s fails to apply to any (sub-)term.
     */
    def outermost (s : Strategy) : Strategy = 
        repeat (oncetd (s))        
    
    /**
     * Construct a strategy that applies s repeatedly to the innermost
     * (i.e., lowest and left-most) (sub-)term to which it applies.
     * Stop with the current term if s doesn't apply anywhere.
     */
    def innermost (s : => Strategy) : Strategy =
        bottomup (attempt (s <* innermost (s)))
    
    /**
     * Construct a strategy that applies s in a top-down fashion, stopping
     * at a frontier where s applies.
     */
    def alltd (s : => Strategy) : Strategy =
        s <+ all (alltd (s))
        
    /**
     * Construct a strategy that succeeds if t is a subterm  of the subject
     * term.  The subject term is unchanged.
     */
    def issubterm (t : Term)  : Strategy =
        oncetd (rule { case `t` => t })
        
    /**
     * Construct a strategy that applies s at every term in a bottom-up fashion
     * regardless of failure.  (Sub-)terms for which the strategy fails are left unchanged.
     */
    def everywherebu (s : => Strategy) : Strategy =
        bottomup (attempt (s))
 
    /**
     * Construct a strategy applies s at every term in a top-down fashion regardless
     * of failure.  (Sub-)terms for which the strategy fails are left unchanged.
     */  
    def everywheretd (s : => Strategy) : Strategy =
        topdown (attempt (s))
    
}
