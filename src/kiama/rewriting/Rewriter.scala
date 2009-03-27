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
        def + (q : => Strategy) : PlusStrategy =
            new PlusStrategy (p, q)
            
        /**
         * Conditional choice.
         *
         * @see <(PlusStrategy)
         */
        @deprecated
        def <++ (l : => Strategy, r: => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => l (t2)
                        case None      => r (t1)
                    }
            }
        
        /**
         * Conditional choice: c < l + r.
         * Construct a strategy that first applies this
         * strategy (c). If it succeeds, apply l to the resulting term,
         * otherwise apply r to the original subject term.
         */
        def < (q : => PlusStrategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    p (t1) match {
                        case Some (t2) => q.lhs (t2)
                        case None      => q.rhs (t1)
                    }
            }

    }
        
    /**
     * Helper class to contain commonality of choice in non-deterministic
     * choice operator and then-else part of a conditional choice.
     */
    class PlusStrategy (p : => Strategy, q : => Strategy) extends Strategy {
        val lhs = p
        val rhs = q
        def apply (t : Term) = (p <+ q) (t)
    }
    
    /**
     * A strategy that always fails.  Stratego's fail is avoided here to
     * avoid a clash with JUnit's method of the same name.
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
     * Traversal to a single child.  Construct a strategy that applies s to
     * the ith child of the subject term (counting from one).  If s succeeds on
     * the ith child producing t, then succeed, forming a new term that is the
     * same as the original term except that the ith child is now t.  If s fails
     * on the ith child or the subject term does not have an ith child, then fail.
     * child (i, s) is equivalent to Stratego's i(s) operator.
     */
    def child (i : Int, s : Strategy) : Strategy =
        new Strategy {
            def apply (t : Term) : Option[Term] = {
                if (debug)
                    println ("child (" + i + "): " + t)
                t match {
                    case p : Product =>
                        val numchildren = p.productArity
                        if ((i < 1) || (i > numchildren)) {
                            if (debug)
                                println ("child (" + i + ") failure (no ith child): " + p)
                            None
                        } else {
                            p.productElement (i-1) match {
                                case ct : Term =>
                                    val children = new Array[AnyRef](numchildren)
                                    for (j <- 0 until numchildren)
                                        children (j) = makechild (p.productElement (j))
                                    s (ct) match {
                                        case Some (ti) =>
                                            children (i-1) = ti
                                        case None      =>
                                            if (debug)
                                                println ("child (" + i + ") failure")
                                            return None
                                    }
                                    val ret = dup (p, children)
                                    if (debug)
                                        println ("child (" + i + ") success: " + ret)
                                    Some (ret)                                    
                                case ci =>
                                    if (debug)
                                        println ("child (" + i + ") failure (not term): " + ci)
                                    None
                            }
                        }
                    case a =>
                        if (debug)
                            println ("child (" + i + ") failure (any): " + a)
                        None
                }
                
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
     * Construct a strategy that repeatedly applies s until it fails and
     * then terminates with application of c.
     */ 
    def repeat (s : => Strategy, c : => Strategy) : Strategy =
        (s <* repeat (s, c)) <+ c

    /**
     * Construct a strategy that applies s repeatedly exactly n times. If
     * s fails at some point during the n applications, the entire strategy 
     * fails. The result of the strategy is that of the nth application of s.
     */
    def repeat (s : => Strategy, n : Int) : Strategy =
        if (n == 0) id else s <* repeat (s, n - 1)

    /**
     * Construct a strategy that repeatedly applies s (at least once) and
     * terminates with application of c.
     */ 
    def repeat1 (s : => Strategy, c : => Strategy) : Strategy =
        s <* (repeat1 (s, c) <+ c)

    /**
     * Construct a strategy that repeatedly applies s (at least once).
     */ 
    def repeat1 (s : => Strategy) : Strategy =
        repeat1 (s, id)

    /**
     * Construct a strategy that repeatedly applies s until c succeeds.
     */
    def repeatuntil (s : => Strategy, c : => Strategy) : Strategy =
        s <* (c <+ repeatuntil (s, c))

    /**
     * Construct a strategy that while c succeeds applies s.  This operator
     * is called "while" in the Stratego library.
     */
    def loop (c : => Strategy, s : => Strategy) : Strategy = 
        attempt (c <* s <* loop (c, s))

    /**
     * Construct a strategy that while c does not succeed applies s.  This
     * operator is called "while-not" in the Stratego library.
     */
    def loopnot (c : => Strategy, s : => Strategy) : Strategy =
        c <+ (s <* loopnot (c, s))

    /**
     * Construct a strategy that applies s at least once and then repeats s
     * while c succeeds.  This operator is called "do-while" in the Stratego
     * library.
     */
    def doloop (s : => Strategy, c : => Strategy) : Strategy =
       s <* loop (c, s)

    /**
     * Construct a strategy that repeats application of s while c fails, after
     * initialization with i.  This operator is called "for" in the Stratego
     * library.
     */
    def loopiter (i : => Strategy, c : => Strategy, s : => Strategy) : Strategy = 
        i <* loopnot (c, s)

    /**
     * Construct a strategy that applies s (i) for each integer i from low to
     * up (inclusive).  This operator is called "for" in the Stratego library.
     */
    def loopiter (s : Int => Strategy, low : Int, up : Int) : Strategy =
        if (low <= up)
            s (low) <* loopiter (s, low + 1, up)
        else
            id
    
    /**
     * Construct a strategy that applies s, then fails if s succeeded or, if s
     * failed, succeeds with the subject term unchanged,  I.e., it tests if
     * s applies, but has no effect on the subject term.
     */
    def not (s : => Strategy) : Strategy =
        s < failure + id
        
    /**
     * Construct a strategy that applies s for its side-effects and always
     * succeeds with the subject term unchanged.  This strategy is similar 
     * to Stratego's "where", except that in this version any effects on
     * bindings are not visible outside s. 
     */
    def where (s : => Strategy) : Strategy =
        rule { case t => s (t); t }
                
    /**
     * Construct a strategy that applies s for its side-effects and always
     * succeeds with the subject term unchanged.  A synonym for where.
     */
    def test (s : => Strategy) : Strategy =
        where (s)

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
    def outermost (s : => Strategy) : Strategy = 
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
     * term.  The subject term is unchanged.  This operator is slightly 
     * different to Stratego's, which takes a tuple of terms.
     */
    def issubterm (t : Term) : Strategy =
        oncetd (rule { case `t` => t })

    /**
     * Construct a strategy that succeeds if t is a subterm  of the subject
     * term but is not equal to the subject term.  The subject term is unchanged.
     * This operator is slightly different to Stratego's, which takes a tuple
     * of terms.
     */
    def ispropersubterm (t : Term)  : Strategy =
        rule {
            case `t` => failure
            case u   => 
        }
        
  /**
   * Succeeds if the first argument (x) is a subterm of the second (y) and x is not y.
   *
   * @type  (a, b) -> (a, b)
   */
//   is-proper-subterm =
//     ?(x, y); not(eq); is-subterm
// 
// strategies

  /**
   * Succeeds if the first argument (x) is a superterm of the second (y).
   *
   * @type  (a, b) -> (a, b)
   */
  // is-superterm =
  //   ?(x, y); where(<oncetd(?y)> x)

  /**
   * Succeeds if the first argument (x) is a superterm of the second (y) and x is not y.
   *
   * @type  (a, b) -> (a, b)
   */
//   is-proper-superterm =
//     ?(x, y); not(eq); is-superterm
// 
// strategies
// 
//   is-proper-subterm-set =
//     ?([y|_], xs); where(<fetch(not(?y); oncetd(?y))> xs)
// 
//   is-proper-superterm-set =
//     ?([x|_], ys); where(<fetch(<is-proper-superterm>(x,<id>))> ys)
// 
// strategies


  /**
   * Succeeds if the current term has no direct subterms.
   *
   * @type  a -> a
   */
  // is-leaf = 
  //   all(fail)

  /**
   * Succeeds if the current term has at least one direct subterm.
   *
   * @type  a -> a
   */
  // is-inner-node =
  //   one(id)
        
    /**
     * Construct a strategy that applies s at every term in a bottom-up fashion
     * regardless of failure.  (Sub-)terms for which the strategy fails are left unchanged.
     */
    def everywherebu (s : => Strategy) : Strategy =
        bottomup (attempt (s))
 
    /**
     * Construct a strategy that applies s at every term in a top-down fashion regardless
     * of failure.  (Sub-)terms for which the strategy fails are left unchanged.
     */  
    def everywheretd (s : => Strategy) : Strategy =
        topdown (attempt (s))
        
   /**
    * Apply restoring action 'rest' if s fails, and then fail.
    * Typically useful if s performs side effects that should be
    * restored/undone in case s fails.
    */
    def restore (s : => Strategy, rest : => Strategy) : Strategy = 
        s <+ (rest <* failure)        

    /**
     * Apply restoring action 'rest' after s terminates, and preserve
     * success/failure behaviour of s.
     * Typically useful if s performs side effects that should be
     * restored always, e.g., when maintaining scope information.
     */
    def restorealways (s : => Strategy, rest : => Strategy) : Strategy = 
        s < rest + (rest <* failure)

    /**
     * Applies s followed by f whether s failed or not.  
     * This operator is called "finally" in the Stratego library.     
     */
    def lastly (s : => Strategy, f : => Strategy) : Strategy = 
        s < where (f) + (where (f) <* failure)

    /**
     * ior (s1, s2) implements 'inclusive or', that is, the
     * inclusive choice of s1 and s2. It first tries s1, if
     * that fails it applies s2 (just like s1 <+ s2). However,
     * when s1 succeeds it also tries to apply s2.
     * The results of the transformations are returned.
     */
    def ior (s1 : => Strategy, s2 : => Strategy) : Strategy = 
        (s1 <* attempt (s2)) <+ s2

    /**
     * or (s1, s2) is similar to ior (s1,s2), but the application
     * of the strategies is only tested.
     */
    def or (s1 : => Strategy, s2 : => Strategy) : Strategy =
        where (s1) < attempt (test (s2)) + test (s2)

    /**
     * and (s1, s2) applies s1 and s2 to the current
     * term and succeeds if both succeed. s2 will always
     * be applied, i.e., and is *not* a short-circuit 
     * operator
     */
    def and (s1 : => Strategy, s2 : => Strategy) : Strategy = 
        where (s1) < test (s2) + (test (s2) <* failure)
        
/**
 * The memo operator makes a strategy
 * into a memoizing strategy that looks up the term to be transformed
 * in a memo table and only computes the transformation if the
 * term is not found.
 */
// module strategy/general/memo
// imports
//   lang/dynamic-rules
// 
// strategies
// 
//   memo-scope(s) = {| Memo: s |}

 /**
  * <memo(tbl, s)> t first looks up the term t in the memo table. If 
  * present the association in the table is produced, else the result 
  * of <s> t is computed and stored in the table.
  */
  // memo(s) :
  //   t -> t'
  //   where ( <Memo> t => t' )
  //      <+ ( <s> t => t'; rules(Memo: t -> t') )
  // 
  // reduce(s) = 
  //   repeat(rec x(some(x) + s))
  // 
  // outermost(s) = 
  //   repeat(oncetd(s))
  // 
  // innermost'(s) = 
  //   repeat(oncebu(s))
  // 
  // innermost(s)  = 
  //   bottomup(try(s; innermost(s)))
  // 
  // innermost-old(s) =
  //   rec x(all(x); (s; x <+ id))
  // 
  // pseudo-innermost3(s) =
  //   rec x(all(x); rec y(try(s; all(all(all(y); y); y); y)))
  // 
  // innermost-memo(s) =
  //   rec x(memo(all(x); (s; x <+ id)))

 /**
  * innermost-tagged(s) reduces the subject term by applying s to
  * innermost redices first. Terms in normal form are tagged (using
  * attributes) to prevent renormalization.
  */
  // innermost-tagged(s : a -> a) = // : a -> a
  //   where(new => tag);
  //   rec x(?_{tag} <+ (all(x); (s; x <+ !<id>{tag})));
  //   bottomup(?<id>{tag})

  /** Apply strategy s to each term in a top down, left to right, (prefix)
   * order, but stop traversal when stop succeeds.
   *
   * @param s         Term -> Term
   * @param stop      (a -> a) * a -> a
   * @type Term -> Term
   * @see topdown
   */
  // topdownS(s, stop: (a -> a) * a -> a) = 
  //   s
  //   ; (stop(topdownS(s,stop)) <+ all(topdownS(s,stop)))

  /** Apply strategy s to each term in a bottom up, left to right,  (postfix)
   * order, but stop traversal when stop succeeds.
   *
   * @param s         Term -> Term
   * @param stop      (a -> a) * a -> a
   * @type Term -> Term
   * @see bottomup
   */
  // bottomupS(s, stop: (a -> a) * a -> a) = 
  //   (stop(bottomupS(s, stop)) <+ all(bottomupS(s, stop)))
  //   ; s

  /** Apply strategy s to all terms when going down then when going up,
   * but stop traversal when stop succeeds.
   *
   * @param s         Term -> Term
   * @param stop      (a -> a) * a -> a
   * @type Term -> Term
   * @see downup
   */
  // downupS(s, stop: (a -> a) * a -> a) = 
  //   s
  //   ; (stop(downupS(s, stop)) <+ all(downupS(s, stop))); s

  /** Apply strategy s1 to all terms going down then apply s2 to all terms
   * when going up, but stop travesal when stop succeeds.
   *
   * @param s1         Term -> Term
   * @param s2         Term -> Term
   * @param stop      (a -> a) * a -> a
   * @type  Term -> Term
   * @see downup
   */
  // downupS(s1, s2, stop: (a -> a) * a -> a) = 
  //   s1
  //   ; (stop(downupS(s1, s2, stop)) <+ all(downupS(s1, s2, stop)))
  //   ; s2

  /**
   * A unit for topdownS, bottomupS and downupS. For example, 
   * topdown(s) is equivalent to topdownS(s,don't-stop).
   *
   * @param s Term -> Term
   * @type _ -> fail
   * @see topdownS
   * @see bottomupS
   * @see downupS
   */
  // don't-stop(s) =
  //   fail

  /**
   * A variation on bottomup is a traversal that also provides the
   * original term as well as the term in which the direct subterms
   * have been transformed. (also known as a paramorphism?)
   */
  // bottomup-para(s) = 
  //   !(<id>, <all(bottomup-para(s))>)
  //   ; s

/**
 * Traversal of a term along a spine.
 *
 * A spine of a term is a chain of nodes from the root to some
 * subterm. 'spinetd' goes down one spine and applies 's' along
 * the way to each node on the spine. The traversal stops when
 * 's' fails for all children of a node.
 */

  /**
   * Apply s along the spine of a term, in top down order. 
   *
   * A spine of a term is a chain of nodes from the root to some
   * subterm. The traversal stops when 's' fails for all children
   * of a node.
   *
   * @param s          Term -> Term
   * @type  Term -> Term
   */
  // spinetd(s) = 
  //   s; try(one(spinetd(s)))

  /**
   * Apply s along the spine of a term, in bottom up order. 
   *
   * A spine of a term is a chain of nodes from the root to some
   * subterm. The traversal stops when 's' fails for all children
   * of a node.
   *
   * @param s          Term -> Term
   * @type  Term -> Term
   */
  // spinebu(s) = 
  //   try(one(spinebu(s))); s
  // 
  // spinetd'(s) = 
  //   s; (one(spinetd'(s)) + all(fail))
  // 
  // spinebu'(s) = 
  //   (one(spinebu'(s)) + all(fail)); s


/**
 * Apply s everywhere along all spines where s applies.
 */

  /** Apply s everywhere along all spines where s applies, in top down
   * order.
   *
   * @param s          Term -> Term
   * @type  Term -> Term 
   * @see   spinetd
   */
  // somespinetd(s) = 
  //   rec x(s; try(some(x)))

  /** Apply s everywhere along all spines where s applies, in bottom
   * up order.
   *
   * @param s          Term -> Term
   * @type  Term -> Term 
   * @see   spinetd
   */
  // somespinebu(s) = 
  //   rec x(try(some(x)); s)

/**
 * Apply s at one position. One s application has to succeed.
 */

  /** Apply s at one position inside a term, in top down order. Once
   * s has succeeded, the traversal stops. If s never succeeds, this
   * strategy fails.
   *
   * @param s          Term -> Term
   * @type  Term -> Term 
   */
  // oncetd(s) = 
  //   rec x(s <+ one(x))

  /** Apply s at one position inside a term, in bottom up order. Once
   * s has succeeded, the traversal stops. If s never succeeds, this
   * strategy fails.
   *
   * @param s          Term -> Term
   * @type  Term -> Term 
   */
  // oncebu(s) = 
  //   rec x(one(x) <+ s)
  // 
  // oncetd-skip(s, skip: (a -> a) * a -> a) = 
  //   rec x(s <+ skip(x) <+ one(x))

/**
 * Apply s at some positions, but at least one.
 *
 * As soon as one is found, searching is stopped, i.e., in the top-down case
 * searching in subtrees is stopped, in bottom-up case, searching
 * in upper spine is stopped.
 */	

  /** Apply s at some positions inside a term, at least once, in top
   * down order. Once s succeeds, the traversal stopped.
   *
   * @param s          Term -> Term
   * @type  Term -> Term
   * @see oncetd
   */
  // sometd(s) = 
  //   rec x(s <+ some(x))

  /** Apply s at some positions inside a term, at least once, in bottom
   * up order. Once s succeeds, the traversal stopped.
   *
   * @param s          Term -> Term
   * @type  Term -> Term
   * @see oncetd
   */
  // somebu(s) = 
  //   rec x(some(x) <+ s)

/**
 * Frontier
 *
 * Find all topmost applications of 's'
 */

  /** Find all topmost applications of s. 
   *
   * @param s          Term -> Term
   * @type  Term -> Term
   */
  // alltd(s) = 
  //   rec x(s <+ all(x))
  // 
  // alldownup2(s1, s2) = 
  //   rec x((s1 <+ all(x)); s2)
  // 
  // alltd-fold(s1, s2) = 
  //   rec x(s1 <+ all(x); s2)
	
/**
 * Leaves
 */
// strategies
// 
//   leaves(s, is-leaf, skip: (a -> a) * a -> a) =
//     rec x((is-leaf; s) <+ skip(x) <+ all(x))
// 
//   leaves(s, is-leaf) =
//     rec x((is-leaf; s) <+ all(x))

/**
 * Find as many applications as possible, but at least one.
 */

  /** Apply s as many times as possible, but at least once, in bottom up
   * order.
   *
   * @param s          Term -> Term
   * @type  Term -> Term
   */
  // manybu(s) = 
  //   rec x(some(x); try(s) <+ s)

  /** Apply s as many times as possible, but at least once, in top down
   * order.
   *
   * @param s          Term -> Term
   * @type  Term -> Term
   */
  // manytd(s) = 
  //   rec x(s; all(try(x)) <+ some(x))


  // somedownup(s) = 
  //   rec x(s; all(x); try(s) <+ some(x); try(s))

  /** Apply s to a term in breadth first order. 
   *
   * @param s          Term -> Term
   * @type  Term -> Term
   */
  // breadthfirst(s) = 
  //   rec x(all(s); all(x))

}
