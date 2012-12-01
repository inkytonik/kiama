/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2012 Anthony M Sloane, Macquarie University.
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
package L3.source

trait PrettyPrinter extends L2.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.{IdnDef, IdnUse, SourceASTNode}

    override def toDoc (n : SourceASTNode) : Doc =
        n match {
            case ProcDecl (IdnDef (i1), as, b, IdnUse (i2)) =>
                "PROCEDURE" <+> i1 <> paramsToDoc (as map toDoc, semi) <> semi <@>
                blockToDoc (b, true) <+> i2 <> semi

            case FPSection (m, ids, t) =>
                val mode : Doc = if (m == VarMode ()) "VAR " else empty
                mode <> idlistToDoc (ids) <+> colon <+> toDoc (t)

            case Call (IdnUse (i), ps) =>
                i <> paramsToDoc (ps map toParenDoc, comma)

            case _ =>
                super.toDoc (n)
        }

    def paramsToDoc (ds : List[Doc], sep : Doc) : Doc =
        ds match {
            case Nil => empty
            case _   => space <> parens (hsep (ds, sep))
        }

}
