package org.kiama
package example.oberon0
package base

trait Analyser {

    import source.SourceASTNode

    /**
     * Check an AST node for semantic errors. Report any errors using the
     * messaging module. This default implementation just ask the node's
     * children to check themselves.
     */
    def check (n : SourceASTNode) {
        for (child <- n.children)
            check (child.asInstanceOf[SourceASTNode])
    }

}
