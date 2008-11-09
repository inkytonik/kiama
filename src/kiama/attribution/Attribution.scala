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
    
package kiama.attribution

/**
 * Support for attribution of syntax trees in a functional style.
 */
object Attribution {
  
    /**
     * Common functionality for all classes that can be attributed.  This trait
     * must be extended by all such classes, which must also implement Product.
     * In practice, this means that they are usually case classes.
     */
    trait Attributable extends Product {
  
        /**
         * A link to the parent attributable node of this node or null if this
         * node has no parent.
         */
        var parent : Attributable = null
            
        /**
         * Is this node the root of the hierarchy?
         */
        def isRoot : Boolean = parent == null
          
        /**
         * If this node is a member of a sequence, a link to the previous
         * node in the sequence.  Null if this is the first node of the
         * sequence, or if it is not a member of a sequence.
         */          
        def prev : this.type = _prev.asInstanceOf[this.type]
        
        /**
         * Private field backing prev to make the types work correctly.
         */
        private var _prev : Attributable = null

        /**
         * If this node is a member of a sequence, a link to the next
         * node in the sequence.  Null if this is the first node of the
         * sequence, or if it is not a member of a sequence.
         */          
        def next : this.type = _next.asInstanceOf[this.type]

        /**
         * Private field backing next to make the types work correctly.
         */
        var _next : Attributable = null

        /**
         * If this node is in a sequence, is it the first element?
         * Otherwise, true.
         */
        def isFirst : Boolean = prev == null
        
        /**
         * If this node is in a sequence, is it the last element?
         * Otherwise, true.
         */
        def isLast : Boolean = next == null
                
        /**
         * If this node is in a sequence, which child number is it
         * (counting from zero)?  Otherwise, zero.
         */             
        var index : Int = 0
        
        /**
         * House-keeping method to connect my children to me and their siblings.
         */
        private def setChildConnections = {
          
            for (i <- 0 until productArity) {
                productElement (i) match {
                    case c : Attributable =>
                        c.parent = this
                    case s : Seq[_] => {
                        var prev : Attributable = null
                        for (i <- 0 until s.length) {
                            s (i) match {
                                case c : Attributable =>
                                    // Bypass Seq node in parent relation
                                    c.parent = this
                                    // Set sequence element properties
                                    c.index = i
                                    c._prev = prev
                                    if (prev != null) prev._next = c
                                    prev = c
                                case _ =>
                                    // Ignore elements that are non-Attributables
                            }                            
                        }
                    }
                    case _ =>
                        // Ignore children that are not Attributable or sequences
                }
            }
            
        }
        
        setChildConnections
        
    }
          
    /**
     * An attribute of a node type T with value of type U, supported by a memo
     * table and circularity test.  The value of the attribute is computed by
     * the function f.  The result is memoised so that it is only evaluated once.
     * f should not itself require the value of this attribute. If it does, a
     * circularity error is reported.
     */
    class Attribute[T,U] (f : T => U) extends (T => U) {

        /**
         * The memo table for this attribute, with memo(t) = Some(v) representing
         * the node t having the value v.  memo(t) = None means that the
         * attribute for t is currently being evaluated.
         */
        private val memo = new scala.collection.mutable.HashMap[T,Option[U]]

        /**
         * Return the value of this attribute for node t, raising an error if
         * it depends on itself.
         */
        def apply (t : T) : U = {
            if (memo contains t) {
                memo (t) match {
                    case Some (u) => u
                    case None     => error ("attribution circularity detected")
                }
            } else {
                memo += (t -> None)
                val u = f (t)
                memo += (t -> Some (u))
                u
            }
        }

    }
    
    /**
     * Global state for the circular attribute evaluation algorithm.
     */
    private object State {
        var IN_CIRCLE = false
        var CHANGE = false
    }
    
    /**
     * An attribute of a node type T with value of type U which has a circular
     * definition.  The value of the attribute is computed by the function f
     * which may itself use the value of the attribute.  init specifies an 
     * initial value for the attribute.  The attribute (and any circular attributes
     * on which it depends) are evaluated until no value changes (i.e., a fixed
     * point is reached).  The final result is memoised so that subsequent evaluations
     * return the same value.
     * 
     * This code implements the basic circular evaluation algorithm from "Circular
     * Reference Attributed Grammars - their Evaluation and Applications", by Magnusson
     * and Hedin from LDTA 2003.
     */
    class CircularAttribute[T,U] (init : U, f : T => U) extends (T => U) {
      
        /**
         * Has the value of this attribute for a given tree already been computed?
         */
        private val computed = new scala.collection.mutable.HashSet[T]
        
        /**
         * Has the attribute for given tree been computed on this iteration of the
         * circular evaluation?
         */
        private val visited = new scala.collection.mutable.HashSet[T]
        
        /**
         * The memo table for this attribute.
         */
        private val memo = new scala.collection.mutable.HashMap[T,U]
        
        /**
         * Return the value of the attribute for tree t, or the initial value if
         * no value for t has been computed.
         */
        private def value (t : T) : U =
            memo.get(t) match {
                case Some (u) => u
                case None     => init
            }

        /**
         * Return the value of this attribute for node t.  Essentially Figure 6
         * from the CRAG paper.
         */
        override def apply (t : T) : U = {
            if (computed contains t) { 
                value (t)
            } else if (!State.IN_CIRCLE) {
                State.IN_CIRCLE = true
                visited += t
                var u = init
                do {
                    State.CHANGE = false
                    val newu = f (t)
                    if (u != newu) {
                        State.CHANGE = true
                        u = newu
                    }
                } while (State.CHANGE)
                visited -= t
                computed += t
                memo += (t -> u)
                State.IN_CIRCLE = false
                u
            } else if (! (visited contains t)) {
                visited += t
                var u = value (t)
                val newu = f (t)
                if (u != newu) {
                    State.CHANGE = true
                    u = newu
                    memo += (t -> u)
                }
                visited -= t
                u            
            } else
                value (t)
        }
        
    }
    
    /**
     * Define an attribute of T nodes of type U by the function f, which 
     * should not depend on the value of this attribute.
     */
    def attr[T,U] (f : T => U) : T => U =
        new Attribute (f)
    
    /**
     * Define a circular attribute of T nodes of type U by the function f.
     * f is allowed to depend on the value of this attribute, which will be
     * given by init initially and will be evaluated iteratively until a
     * fixed point is reached (in conjunction with other circular attributes
     * on which it depends).
     */
    def circular[T,U] (init : U) (f : T => U) : T => U =
        new CircularAttribute (init, f)
    
    /**
     * Define an attribute of T nodes of type U given by the constant value u.
     */
    def constant[T,U] (u : U) : T => U =
        new Attribute (_ => u)

}
