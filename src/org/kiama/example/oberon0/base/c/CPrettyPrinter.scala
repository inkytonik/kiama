package org.kiama
package example.oberon0
package base.c

import org.kiama.util.ParenPrettyPrinter

/**
 * Interface for all C pretty-printers.
 */
trait CPrettyPrinter extends ParenPrettyPrinter {

    this : org.kiama.util.PrettyPrinter =>

    def toDoc (n : CASTNode) : Doc

}
