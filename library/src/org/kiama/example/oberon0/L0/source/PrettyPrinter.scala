/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2013 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

package org.kiama
package example.oberon0
package L0.source

trait PrettyPrinter extends base.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.{Block, Declaration, Identifier, IdnDef,
        SourceTree}
    import org.kiama.output.PrettyExpression
    import scala.collection.immutable.Seq

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

    def declsToDoc (ds : Seq[Declaration]) : Doc =
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

    def optSectionToDoc (section : String, optds : Option[Seq[Declaration]]) : Doc =
        (section, optds) match {
            case (_, None)       => empty
            case ("", Some (ds)) => nest (line <> vsep (ds map toDoc, line)) <> line
            case (s, Some (ds))  => nest (line <> s <> semisep (ds, empty)) <> line
        }

    override def toDoc (n : SourceTree) : Doc =
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

    def idlistToDoc (ids : Seq[IdnDef]) : Doc =
        hsep (ids map toDoc, comma)

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case IntExp (v)  => value (v)
            case IdnExp (id) => id.ident
            case _           => super.toParenDoc (e)
        }

}
