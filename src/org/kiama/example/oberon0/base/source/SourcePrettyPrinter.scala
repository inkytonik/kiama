package org.kiama
package example.oberon0
package base.source

import org.kiama.output.ParenPrettyPrinter

/**
 * Interface for all source pretty-printers.
 */
trait SourcePrettyPrinter extends ParenPrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    def toDoc (n : SourceASTNode) : Doc

}
