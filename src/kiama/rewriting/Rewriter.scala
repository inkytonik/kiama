package kiama.rewriting

import Predef.{ byte2Byte => _, int2Integer => _, _ }

/**
 * Strategy-based term rewriting in the style of Stratego (http://strategoxt.org/).
 */
trait Rewriter {
    
    /**
     * Debug flag. Set this to true in sub-classes or objects to obtain
     * tracing information during rewriting.
     */
    def debug = false

    /**
     * The type of terms that can be rewritten.  Any type of object value is
     * acceptable but generic traversals will only work on Products (eg instances
     * of case classes).
     */
    type Term = AnyRef

    /**
     * Class Strategy represents term-rewriting strategies.
     */
    abstract class Strategy extends Function[Term,Option[Term]] {
        
        self =>
        
        /**
         * Apply this strategy to a term.
         *
         * @param r the subject term
         * @return Some(t) if the strategy succeeded resulting in term t,
         *   or None if the strategy failed
         */
        def apply (r : Term) : Option[Term]
        
        /**
         * Sequential composition.  Create a strategy that first applies
         * this strategy. If it succeeds, then apply the given strategy to the
         * new subject term.  Otherwise fail.
         *
         * @param s the strategy to apply if this strategy succeeds
         * @return the constructed strategy
         */
        def <* (s : => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    self (t1) match {
                        case Some (t2) => s (t2)
                        case None      => None
                    }
            }
    
        /**
         * Deterministic choice.  Create a strategy that first applies
         * this strategy.  If it succeeds, succeed with the resulting term.
         * Otherwise, apply the given strategy to the original subject
         * term.
         *
         * @param s the strategy to apply if this strategy fails
         * @return the constructed strategy
         */
        def <+ (s : => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    self (t1) match {
                        case Some (t2) => Some (t2)
                        case None      => s (t1)
                    }
            }
    
        /**
         * Non-deterministic choice.  Create a strategy that first applies
         * either this strategy or the given strategy.  If it succeeds,
         * succeed with the resulting term. Otherwise, apply the other strategy.
         * Currently implemented as deterministic choice, but this behaviour
         * should not be relied upon.
         *
         * @param s the other strategy to apply
         * @return the constructed strategy
         */
        def + (s : => Strategy) : Strategy =
            <+ (s)
            
        /**
         * Guarded choice.  Create a strategy that first applies this 
         * strategy.  If it succeeds, apply one given strategy to the 
         * resulting term, otherwise apply another given strategy to
         * the original subject term.
         * 
         * @param l the strategy to apply if this strategy succeeds
         * @param r the strategy to apply if this strategy fails
         * @return the constructed strategy
         */
        def <++ (l : => Strategy, r: => Strategy) : Strategy =
            new Strategy {
                def apply (t1 : Term) =
                    self (t1) match {
                        case Some (t2) => l (t2)
                        case None      => r (t1)
                    }
            }

    }
    
    /**
     * Failure. Create a strategy that always fails.
     * 
     * @return the constructed strategy
     */
    val failure : Strategy =
        new Strategy {
            def apply (t : Term) = None
        }
    
    /**
     * Identity.  Create a strategy that always succeeds with the subject
     * term unchanged.
     * 
     * @return the constructed strategy
     */
    val id : Strategy =
        new Strategy {
            def apply (t : Term) = Some (t)
        }
        
    /**
     * Constant strategies.  Any term value implicitly defines a strategy
     * that always succeeds returning that term.
     *
     * @param a the term which should be returned
     * @return the constructed strategy
     */
    implicit def termToStrategy (a : Term) =
        new Strategy {
            def apply (t : Term) = Some (a)
        }
    
    /**
     * Define a rewrite rule.  Create a strategy based on a (possibly partial)
     * function. If the function does not apply, the strategy fails, otherwise
     * the strategy succeeds with the result of the function.
     *
     * @param f the function that defines the behaviour of the strategy
     * @return the constructed strategy
     */
    def rule (f : Term => Term) : Strategy =
        new Strategy {
            def apply (t : Term) =
                try {
                    val t1 = f (t)
                    if (debug) println ("rule success: " + t + " => "+ t1)
                    Some (t1)
                } catch {
                    case _ : scala.MatchError =>
                        if (debug) println ("rule failure: " + t)
                        None
                }
        }

    /**
     * Define a term query.  Create a strategy that always succeeds with no
     * effect on the subject term but applies a given (possibly partial)
     * function to the subject term.  In other words, the strategy runs the
     * function for its side-effects.
     *
     * @param f the function that defines the behaviour of the strategy
     * @return the constructed strategy
     */
    def query[T] (f : PartialFunction[Term,T]) : Strategy =
        rule { case t => f (t); t }

    /**
     * Generic term deconstruction.  An extractor that decomposes Products
     * into the product itself and a sequence of its children.  Terms that
     * are not products are not decomposable (ie the list of children will
     * be empty).
     */
    object Term {
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
     * and the results of folding over the children.  When f is called,
     * the first parameter is the value and the second is a sequence of
     * the values that f has returned for the children.  This will work
     * on any value, but will only decompose Products.  This one is
     * as seen in the Uniplate library.
     *
     * @param f the function to apply at each level
     * @return a function that can be applied to any value to perform the fold
     */
    def para[T] (f : (Any, Seq[T]) => T) : Any => T = {
        case Term (t, ts) => f (t, ts.map (para (f)))
    }

    /**
     * General product duplication function.
     * 
     * @param t the product to duplicate
     * @param children the new children to use
     * @return a product with the same constructor as t but with the new children
     */
    private def dup (t : Product, children : Array[AnyRef]) : Product = {
        val ctor = (t.getClass.getConstructors())(0)
        try {
            ctor.newInstance (children/* : _* */).asInstanceOf[Product]
        } catch {
            case e : java.lang.ClassCastException =>
                error ("dup cast failed: " + t)
            case e : IllegalArgumentException =>
                error ("dup illegal arguments: " + ctor + " " + children.deepMkString (",") +
                       " (expects " + ctor.getParameterTypes.length + ")")
        }
    }

    /**
     * Make a child, checking that it worked properly.
     *
     * @param child the value of the child
     * @return the child converted to an object reference
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
     * Traversal to all children.  Create a strategy that applies a given
     * strategy to all term children of the subject term in left-to-right order.
     * If it succeeds on all of the children, then succeed, forming a new term
     * from the constructor of the original term and the result for each child.
     * If the strategy fails on any child, fail.
     *
     * @param s the strategy to apply
     * @return the constructed strategy
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
     * Traversal to one child.  Create a strategy that applies a given strategy
     * to the term children of the subject term in left-to-right order.  Assume
     * c is the first child on which the strategy succeeds.  Stop applying the 
     * strategy to the children and succeed, forming a new term from the constructor
     * of the original term and the original children, except that c is replaced by
     * the result of applying the strategy to c.  In the event that the strategy
     * fails on all children, then fail.
     *
     * @param s the strategy to apply
     * @return the constructed strategy
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
        
    // End-to-end interface 
    
    /**
     * Rewrite a term.  Apply a strategy to a term returning the result term
     * if the strategy succeeds, otherwise return the original term.
     *
     * @param s the strategy to apply
     * @param t the term to which to apply s
     * @return either the result of applying s, or the original term if s fails
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
     * Collect query results in a Set.  Run a given function as a top-down
     * query on the subject term.  Accumulate the values produced by the
     * function in a set and return the final value of the set.
     * 
     * @param f the function that forms the basis of the query
     * @return a set of the values produced by f
     */
        
    def collects[T] (f : PartialFunction[Term,T]) : Term => Set[T] =
        (t : Term) => {
            var collection = Set[T]()
            def collect = (v : T) => collection += v
            (everywheretd (query (f andThen collect))) (t)
            collection
        }
        
    def collectl[T] (f : PartialFunction[Term,T]) : Term => List[T] =
        (t : Term) => {
            var collection = List[T]()
            def collect = (v : T) => collection = collection ::: List (v)
            (everywheretd (query (f andThen collect))) (t)
            collection
        }
        
    // Counting
    
    def count (f : PartialFunction[Term,Int]) : Term => Int =
        (t : Term) => {
            var total = 0
            def count = (v : Int) => total += v
            (everywheretd (query (f andThen count))) (t)
            total
        }
    
    /**
     * Create a strategy that applies a given strategy, yielding the result
     * of that strategy if it succeeds, or if it fails, leaving the original
     * subject term unchanged.
     *
     * @param s the strategy to apply
     * @return the constructed strategy
     */
    def attempt (s : => Strategy) : Strategy =
        s <+ id
          
    /**
     * Create a strategy that applies a given strategy, repeatedly until it
     * fails.  The 
     
     , yielding the result
     * of that strategy if it succeeds, or if it failed, leaving the original
     * subject term unchanged.
     *
     * @param s the strategy to apply
     * @return the constructed strategy
     */
    def repeat (s : => Strategy) : Strategy =
        attempt (s <* repeat (s))
    
    /**
     * Create a strategy that applies a given strategy, then fails if it
     * succeeded or, if it failed, succeeds with the subject term unchanged, 
     * 
     * @param s the strategy to apply
     * @return the constructed strategy
     */
    def not (s : => Strategy) : Strategy =
        s <++ (failure, id)
        
    /**
     * Create a strategy that applies a given strategy for its side-effects
     * and always succeed with the subject term unchanged.  Note: this
     * is not exactly the same as Stratego's "where" since in this version
     * the environment effects of the strategy are not visible outside.
     *
     * @param s the strategy to apply
     * @return the constructed strategy
     */
    def where (s : => Strategy) : Strategy =
        rule { case t => s (t); t }
    
    def topdown (s : => Strategy) : Strategy =
        s <* all (topdown (s)) 

    def bottomup (s : => Strategy) : Strategy =
        all (bottomup (s)) <* s
  
    def downup (s : => Strategy) : Strategy =
        s <* all (downup (s)) <* s 

    def downup (s1 : => Strategy, s2 : => Strategy) : Strategy =
        s1 <* all (downup (s1, s2)) <* s2

    def downup2 (s1 : => Strategy, s2 : => Strategy) : Strategy =
        s1 <* all (downup2 (s1, s2)) <* s2

    def outermost (s : Strategy) : Strategy = 
        repeat (oncetd (s))        
    
    def innermost (s : => Strategy) : Strategy =
        bottomup (attempt (s <* innermost (s)))

    def oncetd (s : => Strategy) : Strategy =
        s <+ one (oncetd (s))
    
    def oncebu (s : => Strategy) : Strategy =
        one (oncebu (s)) <+ s 
        
    def alltd (s : => Strategy) : Strategy =
        s <+ all (alltd (s))
        
    /**
     * Create a strategy that succeeds if a given term is a subterm
     * of the subject term.  The subject term is unchanged.
     *
     * @param t the term to look for
     * @return the constructed strategy
     */
    def issubterm (t : Term)  : Strategy =
        oncetd (rule { case `t` => t })
        
    // Not sure if these are in the Stratego library.  They are
    // the SYB combinators everywhere and everywhere'.

    /**
     * Create a strategy to apply a given strategy at every term in a
     * bottom-up fashion regardless of failure.  Terms for which the 
     * strategy fails are left unchanged.
     *
     * @param s the strategy to apply
     * @return the constructed strategy
     */
    def everywherebu (s : => Strategy) : Strategy =
        bottomup (attempt (s))
 
    /**
     * Create a strategy to apply a given strategy at every term in a
     * top-down fashion regardless of failure.  Terms for which the 
     * strategy fails are left unchanged.
     *
     * @param s the strategy to apply
     * @return the constructed strategy
     */  
    def everywheretd (s : => Strategy) : Strategy =
        topdown (attempt (s))
    
}
