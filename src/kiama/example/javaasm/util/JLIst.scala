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

/**
 * This file is derived from specifications in "Java and the Java Virtual
 * Machine: Definition, Verification and Validation" by Robert Stärk, Joachim
 * Schmid and Egon Börger, Springer, 2001.
 */
 
package kiama.example.javaasm.util

/**
 * An immutable list type with the interface defined in Section 2.3 of the
 * Stärk et al book.  Note that a JList has its head at the right end.
 */ 
abstract class JList[+T] {
    
    self =>

    /**
     * Store the data as a regular Scala list with the elements reversed.
     */
    val data : List[T]
    
    /**
     * Return the ith element of the list reading from left to right and
     * starting with zero.
     */
    def apply (i : Int) =
        if ((i < 0) || (i > data.length))
            error ("JList.apply: i not in range (0.." + data.length + ") was " + i)
        else
            data (data.length - i - 1)

    /**
     * Concatenate this list with ls.
     */
    def ++[U >: T] (ls : JList[U]) : JList[U] =
        new JList[U] {
            val data = ls.data ++ self.data
        }
            
    /**
     * Produce a printable representation by cheating.
     */
    override def toString : String =
        "J" + data.reverse.toString
    
}

/**
 * JList interface as defined in Section 2.3 of the Stärk et al book. 
 */
object JList {

    /**
     * Create a list with the given elements.
     */
    def apply[T] (ts : T*) : JList[T] =
        new JList[T] {
            val data = ts.toList.reverse
        }

    /**
     * Support for varargs pattern matching.
     */
    def unapplySeq[T] (ls : JList[T]) : Option[Seq[T]] =
        Some (ls.data.reverse)

    /**
     * An empty JList.
     */
    def nil[T] : JList[T] =
        new JList[T] {
            val data = Nil
        }

    /**
     * The number of elements in l.
     */
    def length[T] (ls : JList[T]) : Int =
        ls.data.length
    
    /**
     * Return true if the list is empty, otherwise false.  Note: this
     * function is called "null" in the book which clashes with the
     * Scala null reference.
     */
    def isEmpty[T] (ls : JList[T]) : Boolean =
        ls.data.isEmpty
        
    /**
     * Return a list containing i copies of x.
     */
    def copy[T] (i : Int) (x : T) : JList[T] =
        if (i <= 0)
            error ("JList.copy: count must be positive, was " + i)
        else
            new JList[T] {
                val data = List.make (i, x)
            }
  
    /**
     * Push e onto the right end of ls.
     */
    def push[T] (ls : JList[T], e : T) : JList[T] =
        new JList[T] {
            val data = e :: ls.data
        }
        
    /**
     * Return the right most (the last) element of ls.
     */
    def top[T] (ls : JList[T]) : T =
        if (ls.data == Nil)
            error ("JList.top: applied to empty list")
        else
            ls.data.head

    /**
     * Return the list consisting of the last n elements of ls.
     */
    def take[T] (ls : JList[T], n : Int) : JList[T] =
        if ((n < 0) || (n > length (ls)))
            error ("JList.take: can't take " + n + " elements from a list of length " + length (ls))
        else
            new JList[T] {
                val data = ls.data.take (n)
            }
            
    /**
     * Return ls with its right most (the last) element removed.
     */
    def pop[T] (ls : JList[T]) : JList[T] =
        if (ls.data == Nil)
            error ("JList.pop: applied to empty list")
        else
            new JList[T] {
                val data = ls.data.tail
            }
     
    /**
     * Return the list that is ls with the last n elements removed.
     */
    def drop[T] (ls : JList[T], n : Int) : JList[T] =
        if ((n < 0) || (n > length (ls)))
            error ("JList.drop: can't take " + n + " elements from a list of length " + length (ls))
        else
            new JList[T] {
                val data = ls.data.drop (n)
            }

    /**
     * Split the last n elements of the list ls and return those elements
     * and the remaining list.  I.e., split (ls,n) == (ls2,ns) where
     * ls2 ++ ns == ls and length (ns) == n.
     */
    def split[T] (ls : JList[T], n : Int) : (JList[T], JList[T]) =
        if ((n < 0) || (n > length (ls)))
            error ("JList.split: n not in range (0.." + length (ls) + ") was " + n)
        else {
            val (d1, d2) = ls.data.splitAt (n)
            (new JList[T] { val data = d2 }, new JList[T] { val data = d1 })
        }
        
    /**
     * ns gives a list of sub-list lengths.  Split ls into the sub-lists (from the
     * end) of lengths given by the elements of ns.  I.e., splits (ls, ns) ==
     * (ls2, [ns0, ns1, ...]) where there is one nsi for each length in ns, such 
     * that ls = ls2 ++ ns0 ++ ns1 ++ ... and length (nsi) == ns (i).
     */
    def splits[T] (ls : JList[T], ns : JList[Int]) : (JList[T], JList[JList[T]]) =
        if (isEmpty (ns))
            (ls, nil)
        else {
            val (ls2, el) = split[T] (ls, top (ns))
            val (ls3, els) = splits (ls2, pop (ns))
            (ls3, push (els, el))
        }
        
    /**
     * Return the list which is split off from ls by splits (ls, ns).  I.e.,
     * satisfying (_, nss) = splits (ls, ns).
     */
    def tops[T] (ls : JList[T], ns : JList[Int]) : JList[JList[T]] =
        splits (ls, ns)._2
        
    /**
     * Zip the two lists xs and ys returning a list of list of their corresponding
     * pair elements.
     */
    def zip[T,U] (xs : JList[T], ys : JList[U]) : JList[(T,U)] =
        new JList[(T,U)] {
            val data = xs.data.zip (ys.data)
        }
    
}
    

 