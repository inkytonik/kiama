package org.kiama
package example.oberon0
package base.source

import org.kiama.util.ParenPrettyPrinter

/**
 * Interface for all source pretty-printers.
 */
trait SourcePrettyPrinter extends ParenPrettyPrinter {

    this : org.kiama.util.PrettyPrinter =>

    def toDoc (n : SourceASTNode) : Doc

}
