package org.kiama
package example.oberon0
package base.c

import org.kiama.output.ParenPrettyPrinter

/**
 * Interface for all C pretty-printers.
 */
trait CPrettyPrinter extends ParenPrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    def toDoc (n : CASTNode) : Doc

}
