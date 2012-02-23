package org.kiama
package example.oberon0
package base

/**
 * Interface for all transformers. Also provides operations that are 
 * useful in transformer implementations.
 */
trait Transformer {

    import source.{ModuleDecl, SourceASTNode}
    import org.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}

    /**
     * Transform a module in some way, returning a new module.  By default,
     * just return the given module.
     */
    def transform (m : ModuleDecl) : ModuleDecl =
        m

}
