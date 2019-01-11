/*
 * This file is part of Kiama.
 *
 * Copyright(C) 2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.minijava

object Monto {

    import org.bitbucket.inkytonik.kiama.example.minijava.PrettyPrinter._
    import org.bitbucket.inkytonik.kiama.example.minijava.MiniJavaTree._
    import org.bitbucket.inkytonik.kiama.example.minijava.SemanticAnalyser
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.collectl

    // Name analysis product support

    def nameDocument(tree : MiniJavaTree, analyser : SemanticAnalyser) : Document = {

        import analyser.entity
        import org.bitbucket.inkytonik.kiama.example.minijava.SymbolTable._
        import org.bitbucket.inkytonik.kiama.util.Entity
        import tree.parent

        val bullet = "-"
        val ellipses = "..."

        val ast = tree.root
        val idnuses = collectl { case i : IdnUse => i }(ast)

        def toEnvDoc(t : MiniJavaNode) : Doc =
            t match {
                case Program(m, cs) =>
                    toEnvDoc(m) <> hcat(cs map toEnvDoc)
                case MainClass(i, _) =>
                    idndefToDoc(i) <> line
                case Class(i, _, b) =>
                    idndefToDoc(i) <> nest(toEnvDoc(b)) <> line
                case ClassBody(fs, ms) =>
                    hcat(fs map toEnvDoc) <> hcat(ms map toEnvDoc)
                case Method(i, b) =>
                    line <> idndefToDoc(i) <> nest(toEnvDoc(b))
                case MethodBody(_, as, vs, _, _) =>
                    hcat(as map toEnvDoc) <> hcat(vs map toEnvDoc)
                case Argument(_, i) =>
                    idndefToDoc(i)
                case Field(_, i) =>
                    idndefToDoc(i)
                case Var(_, i) =>
                    idndefToDoc(i)
                case _ =>
                    emptyDoc
            }

        def idndefToDoc(idndef : IdnDef) : Doc = {

            val idn = idndef.idn
            val ent = entity(idndef)

            def kindDeclToDoc(kind : String, decl : MiniJavaNode) : Doc =
                line <> bullet <+> link(decl, kind <+> idn) <> entityUsesToDoc(ent)

            def specialEntityToDoc(kind : String) : Doc =
                line <> bullet <+> link(idndef, kind <+> idn)

            def entityUsesToDoc(ent : Entity) : Doc = {
                val uses = idnuses.filter(entity(_) eq ent)
                if (uses.isEmpty)
                    emptyDoc
                else
                    nest(line <> vsep(uses.map(contextOfUseToDoc)))
            }

            def contextOfUseToDoc(use : IdnUse) : Doc =
                use match {
                    case parent(parent.pair(_ : IdnExp, n @ (_ : Expression | _ : Result | _ : Statement))) =>
                        link(n, summariseParent(n))
                    case parent(n @ (_ : Expression | _ : Statement)) =>
                        link(n, summariseParent(n))
                    case parent(parent(parent.pair(_ : MethodBody, n))) =>
                        link(n, summariseParent(n))
                    case parent(parent.pair(_ : ClassType, n)) =>
                        link(n, summariseParent(n))
                    case parent(n : Class) =>
                        link(n, "class" <+> n.name.idn <+> "extends" <+> use.idn)
                    case _ =>
                        link(use, use.idn)
                }

            def summariseParent(n : MiniJavaNode) : Doc =
                n match {
                    case If(e, _, _) =>
                        "if" <+> parens(toDoc(e)) <+> ellipses
                    case Method(IdnDef(i), MethodBody(t, _, _, _, _)) =>
                        "public" <+> toDoc(t) <+> i <+> ellipses
                    case While(e, _) =>
                        "while" <+> parens(toDoc(e)) <+> ellipses
                    case _ =>
                        toDoc(n)
                }

            ent match {
                case mjent : MiniJavaEntity =>
                    kindDeclToDoc(mjent.productPrefix, mjent.decl)
                case p : Product =>
                    specialEntityToDoc(p.productPrefix)
                case _ =>
                    nest(line <> link(idndef, "???" <+> idn))
            }

        }

        pretty(toEnvDoc(ast))

    }

    // Outline product support

    def outlineDocument(t : MiniJavaNode) : Document = {

        def toOutlineDoc(t : MiniJavaNode) : Doc =
            link(t, toOutlineDocNoLink(t))

        def toOutlineDocNoLink(t : MiniJavaNode) : Doc =
            t match {
                case Program(m, cs) =>
                    toOutlineDoc(m) <@> line <> vsep(cs map toOutlineDoc, line)
                case MainClass(i, _) =>
                    "class" <+> toOutlineDocNoLink(i)
                case Class(i, _, b @ ClassBody(fs, ms)) =>
                    "class" <+> toOutlineDocNoLink(i) <+>
                        (if ((fs == Nil) && (ms == Nil))
                            emptyDoc
                        else
                            braces(nest(line <> toOutlineDocNoLink(b)) <> line))
                case ClassBody(fs, ms) =>
                    (if (fs == Nil)
                        emptyDoc
                    else
                        vsep(fs map toOutlineDoc) <> line) <>
                        vsep(ms map toOutlineDoc)
                case Field(t, i) =>
                    toOutlineDocNoLink(t) <+> toOutlineDocNoLink(i) <> semi
                case Var(t, i) =>
                    toOutlineDocNoLink(t) <+> toOutlineDocNoLink(i) <> semi
                case Method(i, b) =>
                    toOutlineDocNoLink(b.tipe) <+> toOutlineDocNoLink(i) <+> toOutlineDoc(b)
                case MethodBody(_, as, vs, _, _) =>
                    parens(hsep(as map toOutlineDoc, comma)) <>
                        (if (vs == Nil)
                            emptyDoc
                        else
                            space <> braces(nest(line <> vsep(vs map toOutlineDoc)) <> line))
                case Argument(t, i) =>
                    toOutlineDocNoLink(t) <+> toOutlineDocNoLink(i)
                case t : Type =>
                    t.toString
                case n : IdnTree =>
                    n.idn
                case _ =>
                    emptyDoc
            }

        pretty(toOutlineDoc(t))

    }

}
