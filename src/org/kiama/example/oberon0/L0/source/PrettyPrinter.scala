package org.kiama
package example.oberon0
package L0.source

trait PrettyPrinter extends base.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.{Block, Declaration, Identifier, IdnDef,
        SourceASTNode}
    import org.kiama.output.PrettyExpression

    /**
     * Pretty-print a block, omitting the BEGIN if there are no statements.
     * Add the possibility of declarations to the previous level.
     */
    override def blockToDoc (b : Block, beginend : Boolean = false) : Doc = {
        val ss = super.blockToDoc (b, beginend)
        b.decls match {
            case Nil => line <> ss
            case ds  => declsToDoc (ds) <@> ss
        }
    }

    def declsToDoc (ds : List[Declaration]) : Doc =
        if (ds == Nil)
            empty
        else {
            val m = ds.groupBy (declsection)
            optSectionToDoc ("CONST", m.get ("CONST")) <>
            optSectionToDoc ("TYPE", m.get ("TYPE")) <>
            optSectionToDoc ("VAR", m.get ("VAR")) <>
            optSectionToDoc ("", m.get (""))
        }

    override def declsection (d : Declaration) : String =
        d match {
            case _ : ConstDecl => "CONST"
            case _ : TypeDecl  => "TYPE"
            case _ : VarDecl   => "VAR"
            case _             => super.declsection (d)
        }

    def optSectionToDoc (section : String, optds : Option[List[Declaration]]) : Doc =
        (section, optds) match {
            case (_, None)       => empty
            case ("", Some (ds)) => nest (line <> vsep (ds map toDoc, line)) <> line
            case (s, Some (ds))  => nest (line <> s <> semisep (ds, empty)) <> line
        }

    override def toDoc (n : SourceASTNode) : Doc =
        n match {
            case ConstDecl (id, e) =>
                toDoc (id) <+> equal <+> toDoc (e) <> semi

            case TypeDecl (id, t) =>
                toDoc (id) <+> equal <+> toDoc (t) <> semi

            case VarDecl (ids, t) =>
                idlistToDoc (ids) <+> colon <+> toDoc (t) <> semi

            case NamedType (d) =>
                toDoc (d)

            case Assignment (d, e) =>
                toDoc (d) <+> ":=" <+> toDoc (e)

            case e : Expression =>
                toParenDoc (e)

            case i : Identifier =>
                text (i.ident)

            case _ =>
                super.toDoc (n)
        }

    def idlistToDoc (ids : List[IdnDef]) : Doc =
        hsep (ids map toDoc, comma)

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case IntExp (v)  => value (v)
            case IdnExp (id) => id.ident
            case _           => super.toParenDoc (e)
        }

}
