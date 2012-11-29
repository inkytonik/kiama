package org.kiama
package example.oberon0
package base.c

trait PrettyPrinter extends CPrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import org.kiama.output.PrettyExpression

    def toDoc (n : CASTNode) : Doc =
        n match {
            case CProgram (is, ds) =>
                vsep (is map toDoc) <@>
                vsep (ds map toDoc, semi)

            case CInclude (s) =>
                "#include " + s

            case CFunctionDecl (d, args, b) =>
                toDoc (d) <+>
                parens (hsep (args map toDoc, comma)) <+>
                toDoc (b)

            case CBlock (ds, ss) =>
                braces (nest (lterm (ds map toDoc, semi) <>
                              lsep (ss map toDoc, empty)) <>
                        line)

            case CVarDecl (i, t : CArrayType) =>
                basetypeToDoc (t) <> i <> arraydimensToDoc (t)

            case CVarDecl (i, t) =>
                basetypeToDoc (t) <> i

            case CEmptyStmt () =>
                semi

            case CReturn (e) =>
                "return" <+> toDoc (e) <> semi

            case e : CExpression =>
                toParenDoc (e)

            case _ =>
                empty
        }

    def basetypeToDoc (t : CType) : Doc =
        t match {
            case CIntType ()        => "int" <> space
            case CStrType ()        => "char *"
            case CArrayType (_, et) => basetypeToDoc (et)
        }

    def arraydimensToDoc (t1 : CArrayType) : Doc = {
        "[" + t1.size + "]" <>
        (t1.elemtype match {
            case t2 : CArrayType => arraydimensToDoc (t2)
            case _               => empty
         })
    }

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case CIntExp (v) => value (v)
            case _           => super.toParenDoc (e)
        }

}
