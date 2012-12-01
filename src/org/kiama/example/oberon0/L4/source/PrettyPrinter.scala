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
package L4.source

trait PrettyPrinter extends L3.source.PrettyPrinter {

    this : org.kiama.output.PrettyPrinter =>

    import base.source.SourceASTNode
    import org.kiama.output.PrettyExpression

    override def toDoc (n : SourceASTNode) : Doc =
        n match {
            case ArrayTypeDef (s, t) =>
                "ARRAY" <+> toDoc (s) <+> "OF" <+> toDoc (t)

            case RecordTypeDef (Nil) =>
                "RECORD" <+> "END"

            case RecordTypeDef (fs) =>
                "RECORD" <+> hsep (fs map toDoc, semi) <+> "END"

            case FieldList (ids, t) =>
                (hsep (ids map text, comma)) <+> colon <+> toDoc (t)

            case _ =>
                super.toDoc (n)
        }

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case IndexExp (b, e) =>
                toDoc (b) <> brackets (toDoc (e))
            case FieldExp (b, FieldIdn (f)) =>
                toDoc (b) <> "." <> f
            case _ =>
                super.toParenDoc (e)
        }

}
