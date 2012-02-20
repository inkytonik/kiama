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

    /**
     * Make a deep clone of t.  If you have a node whose structure needs to
     * appear more than once in a tree, use this to duplicate it so that the
     * resulting structure is actually a tree, not a graph.
     */
    def clone[T <: SourceASTNode] (t : T) : T = {
        val clone = everywherebu (rule {
                        case n : SourceASTNode if !n.hasChildren => 
                            n.clone ()
                    })
        rewrite (clone) (t)
    }

}
