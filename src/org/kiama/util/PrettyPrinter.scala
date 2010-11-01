/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Dominic R B Verity, Macquarie University.
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
package util

/**
 * Simple class for pretty-printing capabilities.
 */
class PrettyPrinter (
        summaryFlag : Boolean = false,
        val truncate : Int = 3,
        val tabWidth : Int = 4) {

    require(truncate >= 0)
    require(tabWidth >= 0)

    /**
     * Pretty print buffer
     */
    private val buffer : StringBuilder = new StringBuilder

    /**
     * Current tabStop
     */
    private var tabStop : Int = 0

    /**
     * Pretty print an indented block
     */
    def indent (by : Int) (printer : => Unit) {
        val oldTabStop = tabStop
        tabStop += by
        try
            printer
        finally
            tabStop = oldTabStop
    }

    def indent : (=> Unit) => Unit = indent (tabWidth : Int) _

    /**
     * Primitive pretty printers
     */
    def newline { buffer.append("\n") }
    def text (t : String) {
        val l : Int = buffer.length - buffer.lastIndexOf('\n') - 1
        buffer.append("".padTo(tabStop - l, ' '))
        buffer.append(t)
    }
    def untabbedText (t : String) { buffer.append(t) }

    /**
     * Pretty print a list in column format
     */
    def printList[T](
        heading : String = "List", lst : List[T],
        elemPrinter : T => Unit = (v : PrettyPrintable) => v.pretty(this)
    ) {
        text(heading)
        text("(")
        lst match {
            case Nil => text(")")
            case x :: xs => {
                indent {
                    newline
                    elemPrinter(x)
                    val ys = if (summaryFlag) xs.take(truncate) else xs
                    for (y <- ys) {
                        text(",")
                        newline
                        elemPrinter(y)
                    }
                    if (ys.length < xs.length) {
                        text(",")
                        newline
                        text("...")
                    }
                }
                newline
                text(")")
            }
        }
    }

    /**
     * Convert to a string.
     */
    override def toString () = buffer.toString

}

/**
 * Trait to add pretty printing services to a class
 */
trait PrettyPrintable {
    def pretty (p : PrettyPrinter) : Unit = p.text(this.toString)
}


