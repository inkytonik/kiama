/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Anthony M Sloane, Macquarie University.
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
 * Class of objects that can emit code.  By default, code is output to the
 * standard output.  Subclass this if you need it to go somewhere else.
 */
class Emitter {

    /**
     * Emit anything.
     */
    def emit (any : Any) {
        print (any.toString)
    }

    /**
     * Emit anything and start a new line.
     */
    def emitln (any : Any) {
        println (any.toString)
    }

    /**
     * Emit a new line.
     */
    def emitln {
        println
    }

}

/**
* An emitter that records the code in a string that can be accessed
* via the result method.
*/
class StringEmitter extends Emitter {
    val b = new StringBuilder
    override def emit (any : Any) = b.append (any.toString)
    override def emitln (any : Any) = b.append (any.toString).append ('\n')
    override def emitln () = b.append ('\n')
    def result () = b.result
}
