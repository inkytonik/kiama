/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2019-2020 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.minijava

import org.bitbucket.inkytonik.kiama.util.Compiler
import MiniJavaTree.{MiniJavaNode, Program}

/**
 * Language server support for MiniJava.
 */
trait Server {

    this : Compiler[MiniJavaNode, Program] =>

    import org.bitbucket.inkytonik.kiama.example.minijava.MiniJavaTree._
    import org.bitbucket.inkytonik.kiama.example.minijava.PrettyPrinter._
    import org.bitbucket.inkytonik.kiama.example.minijava.SymbolTable._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, emptyDocument}
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.collectl
    import org.bitbucket.inkytonik.kiama.util.{Position, Source}
    import org.eclipse.lsp4j.{DocumentSymbol, SymbolKind}
    import scala.collection.mutable

    /**
     * The analysers previously used by the semantic analysis phase of this
     * compiler, indexed by source name.
     */
    val analysers = mutable.Map[Source, SemanticAnalyser]()

    /**
     * Find nodes that overlap the given position and return the
     * first value that `f` produces when applied to the appropriate
     * analyser and an overlapping node.
     */
    def getRelevantInfo[I](
        position : Position,
        f : (String, SemanticAnalyser, Vector[MiniJavaNode]) ==> Option[I]
    ) : Option[I] =
        for (
            analyser <- analysers.get(position.source);
            nodes = analyser.tree.nodes;
            relevantNodes = positions.findNodesContaining(nodes, position);
            info <- f((position.source.name, analyser, relevantNodes))
        ) yield info

    /**
     * Return applicable code actions for the given position (if any).
     */
    override def getCodeActions(position : Position) : Option[Vector[TreeAction]] =
        getRelevantInfo(position, {
            case (uri, analyser, nodes) =>
                Some(
                    nodes.collect {
                        case n @ If(c, t, e) =>
                            val newNode = If(c, e, t)
                            val newText = PrettyPrinter.format(newNode).layout
                            TreeAction("swap if-statement branches", uri, n, newText)
                    }
                )
        })

    /**
     * Definitions are provided for defined identifiers and point
     * to the corresponding declaration node.
     */
    override def getDefinition(position : Position) : Option[MiniJavaNode] =
        getRelevantInfo(position, {
            case (uri, analyser, nodes) =>
                nodes.collectFirst {
                    case n : IdnTree => n
                }.toList.collectFirst(n =>
                    analyser.entity(n) match {
                        case e : MiniJavaOkEntity =>
                            e.decl
                    })
        })

    /**
     * Return a formatted version of the whole of the given MiniJava source.
     */
    override def getFormatted(source : Source) : Option[String] =
        for (
            analyser <- analysers.get(source);
            formatted = format(analyser.tree.root).layout
        ) yield formatted

    /**
     * Hover information is provided for identifier defs and uses.
     * It consists of a short description of the associated entity
     * and a pretty-printed version of the corresponding declaration.
     */
    override def getHover(position : Position) : Option[String] =
        getRelevantInfo(position, {
            case (uri, analyser, nodes) =>
                nodes.collectFirst {
                    case n : IdnTree =>
                        analyser.entity(n) match {
                            case MultipleEntity() =>
                                s"multiply-defined ${n.idn}"
                            case UnknownEntity() =>
                                s"unknown ${n.idn}"
                            case e : MiniJavaOkEntity =>
                                val p = hoverDocument(e.decl).layout
                                s"${e.desc} ${n.idn}\n\n```\n$p```"
                        }
                }
        })

    /**
     * The identifier definition associated with an entity declaration.
     */
    def idndefOfEntityDecl(entity : MiniJavaEntity) : Option[IdnDef] =
        entity match {
            case MainClassEntity(decl) => Some(decl.name)
            case ClassEntity(decl)     => Some(decl.name)
            case MethodEntity(decl)    => Some(decl.name)
            case ArgumentEntity(decl)  => Some(decl.name)
            case FieldEntity(decl)     => Some(decl.name)
            case VariableEntity(decl)  => Some(decl.name)
            case _ =>
                None
        }

    /**
     * The references to a symbol at a given position.
     */
    override def getReferences(position : Position, includeDecl : Boolean) : Option[Vector[MiniJavaNode]] =
        getRelevantInfo(position, {
            case (uri, analyser, nodes) =>
                nodes.collectFirst {
                    case n : IdnTree => n
                }.toList.collectFirst(n =>
                    analyser.entity(n) match {
                        case e : MiniJavaOkEntity =>
                            val uses = analyser.tree.nodes.collect {
                                case u : IdnUse if analyser.entity(u) == e =>
                                    u
                            }
                            idndefOfEntityDecl(e) match {
                                case Some(decl) if includeDecl =>
                                    decl +: uses
                                case _ =>
                                    uses
                            }
                    })
        })

    /**
     * Convert MiniJava entities into LSP symbol kinds.
     */
    def getSymbolKind(entity : MiniJavaEntity) : Option[SymbolKind] =
        entity match {
            case _ : MainClassEntity | _ : ClassEntity =>
                Some(SymbolKind.Class)
            case _ : MethodEntity =>
                Some(SymbolKind.Method)
            case _ : ArgumentEntity | _ : VariableEntity =>
                Some(SymbolKind.Variable)
            case _ : FieldEntity =>
                Some(SymbolKind.Field)
            case _ =>
                None
        }

    /**
     * The symbols of an entire compilation unit.
     */
    override def getSymbols(source : Source) : Option[Vector[DocumentSymbol]] =
        for (
            analyser <- analysers.get(source);
            nodes = analyser.tree.nodes;
            idndefs = nodes.collect { case n : IdnDef => n };
            symbols = for (
                idndef <- idndefs;
                entity = analyser.entity(idndef);
                kind <- getSymbolKind(entity);
                decl = entity.asInstanceOf[MiniJavaOkEntity].decl;
                detail = hoverDocument(decl).layout
            ) yield new DocumentSymbol(
                idndef.idn, kind, rangeOfNode(decl), rangeOfNode(idndef), detail
            )
        ) yield symbols

    // Name analysis product support

    def nameDocument(tree : MiniJavaTree, analyser : SemanticAnalyser) : Document = {

        import analyser.entity
        import org.bitbucket.inkytonik.kiama.example.minijava.SymbolTable._
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

            def entityUsesToDoc(ent : MiniJavaEntity) : Doc = {
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
                case mjent : MiniJavaOkEntity =>
                    kindDeclToDoc(mjent.productPrefix, mjent.decl)
                case p : Product =>
                    specialEntityToDoc(p.productPrefix)
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

    // Hover support

    def hoverDocument(t : MiniJavaNode) : Document = {

        def toHoverDoc(t : MiniJavaNode) : Doc =
            t match {
                case MainClass(i, _) =>
                    "class" <+> toHoverDoc(i)
                case Class(i, _, b @ ClassBody(fs, ms)) =>
                    "class" <+> toHoverDoc(i)
                case Field(t, i) =>
                    toHoverDoc(t) <+> toHoverDoc(i)
                case Var(t, i) =>
                    toHoverDoc(t) <+> toHoverDoc(i)
                case Method(i, b) =>
                    toHoverDoc(b.tipe) <+> toHoverDoc(i) <+> toHoverDoc(b)
                case MethodBody(_, as, _, _, _) =>
                    parens(hsep(as map toHoverDoc, comma))
                case Argument(t, i) =>
                    toHoverDoc(t) <+> toHoverDoc(i)
                case t : Type =>
                    t.toString
                case n : IdnTree =>
                    n.idn
                case _ =>
                    emptyDoc
            }

        pretty(toHoverDoc(t))

    }

    // Monto product publishing

    def publishTargetProduct(source : Source, document : => Document = emptyDocument) : Unit = {
        if (settingBool("showTarget"))
            publishProduct(source, "target", "jasmin", document)
    }

    def publishTargetTreeProduct(source : Source, document : => Document = emptyDocument) : Unit = {
        if (settingBool("showTargetTree"))
            publishProduct(source, "targettree", "scala", document)
    }

    def publishOutlineProduct(source : Source, document : => Document = emptyDocument) : Unit = {
        if (settingBool("showOutline"))
            publishProduct(source, "outline", "minijava", document)
    }

    def publishNameProduct(source : Source, document : => Document = emptyDocument) : Unit = {
        if (settingBool("showNameAnalysisStructure"))
            publishProduct(source, "name", "minijava", document)
    }

}
