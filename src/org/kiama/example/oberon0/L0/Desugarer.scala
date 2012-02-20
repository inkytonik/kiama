package org.kiama
package example.oberon0
package L0

/**
 * Desugaring transformation for L0.
 */
trait Desugarer extends base.Transformer with NameAnalyser {

    import base.source.{Identifier, IdnDef, IdnUse, ModuleDecl}
    import org.kiama.rewriting.Rewriter.{everywherebu, rewrite, rule}
    import org.kiama.attribution.Attribution.initTree
    import scala.collection.mutable.HashMap

    /**
     * Desugar the provided module to replace identifier uses with uses
     * of unique names. Then call the next level of transformation.
     */
    override def transform (m : ModuleDecl) : ModuleDecl = {
        initTree (m)
        super.transform (uniquifyNames (m))
    }

    /**
     * Rename user-defined names to avoid clashes with outer declarations
     * of the same name.  This transformation is not idempotent.
     */
    def uniquifyNames (m : ModuleDecl) : ModuleDecl = {

        /**
         * The name to use for a particular name occurrence.  If the occurrence
         * denotes a named entity, use that entity's id, otherwise leave the
         * occurrence unchanged.
         */
        def nameOf (i : Identifier, isdef : Boolean) : Identifier =
            (i->entity) match {
                case e : Named => if (isdef) IdnDef (e.id) else IdnUse (e.id)
                case _         => i
            }

        /**
         * Rename any user-defined name that is used more than once to its id
         * if it has one.
         */
        val renameNames =
            everywherebu (
                rule {
                    case i : IdnDef => nameOf (i, true)
                    case i @ IdnUse (s) => 
                        (i->entity) match {
                            case b : Builtin => i
                            case _           => nameOf (i, false)
                        }
                }
            )

        rewrite (renameNames) (m)

    }

}
