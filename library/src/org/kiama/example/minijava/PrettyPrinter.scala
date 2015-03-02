/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014-2015 Anthony M Sloane, Macquarie University.
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
package example.minijava

/**
 * Abstract syntax tree pretty-printing for Minijava.
 */
class PrettyPrinter extends org.kiama.output.ParenPrettyPrinter {

    import MiniJavaTree._
    import org.kiama.output.PrettyExpression
    import org.kiama.output.PrettyPrinterTypes.Document
    import scala.collection.immutable.Seq

    /**
     * Format a MiniJava node.
     */
    def format (t : MiniJavaNode) : Document =
        pretty (toDoc (t), 5)

    /**
     * Convert a MiniJava AST node to a pretty-printing document.
     */
    def toDoc (t : MiniJavaNode) : Doc =
        positioned (t, toDocPositioned (t))

    def toDocPositioned (t : MiniJavaNode) : Doc =
        t match {
            case Program (m, cs) =>
                toDoc (m) <> ssep (cs map toDoc, line <> line)
            case MainClass (i, s) =>
                "class" <+> toDoc (i) <+> braces (
                    nest (
                        line <>
                        "public static void main ()" <+> braces (
                            nest (
                                line <>
                                toDoc (s)
                            ) <>
                            line
                        )
                    ) <>
                    line
                ) <>
                line <>
                line
            case Class (i, optsc, b) =>
                val ext = optsc.map (n => "extends" <+> toDoc (n)).getOrElse (empty)
                "class" <+> toDoc (i) <> ext <+> braces (
                    nest (
                        line <>
                        toDoc (b)
                    ) <>
                    line
                ) <>
                line <>
                line
            case ClassBody (fs, ms) =>
                vsep (fs map toDoc) <@> vsep (ms map toDoc)
            case Field (t, i) =>
                toDoc (t) <+> toDoc (i) <> semi
            case Var (t, i) =>
                toDoc (t) <+> toDoc (i) <> semi
            case Method (i, b) =>
                line <> "public" <+> toDoc (b.tipe) <+> toDoc (i) <+> toDoc (b)
            case MethodBody (_, as, vs, ss, r) =>
                parens (hsep (as map toDoc, comma)) <>
                    line <>
                    braces (
                        nest (bodyToDoc (vs, ss, r)) <>
                        line
                    )
            case Argument (t, i) =>
                toDoc (t) <+> toDoc (i)
            case t : Type =>
                t.toString
            case Block (ss) =>
                braces (
                    nest (
                        line <>
                        vsep (ss map toDoc)
                    ) <>
                    line
                )
            case If (e, s1, s2) =>
                "if" <+> parens (toDoc (e)) <>
                    nest (
                        line <>
                        toDoc (s1)
                    ) <>
                    line <>
                    "else" <>
                    nest (
                        line <>
                        toDoc (s2)
                    )
            case While (e, s) =>
                "while" <+> parens (toDoc (e)) <+> toDoc (s)
            case Println (e) =>
                "System.out.println" <+> parens (toDoc (e)) <> semi
            case VarAssign (i, e) =>
                toDoc (i) <+> equal <+> toDoc (e) <> semi
            case ArrayAssign (i, e1, e2) =>
                toDoc (i) <> brackets (toDoc (e1)) <+> equal <+> toDoc (e2) <> semi
            case n : IdnTree =>
                n.idn
            case e : PrettyExpression =>
                toParenDoc (e)
        }

    def bodyToDoc (vs : Seq[Var], ss : Seq[Statement], r : Expression) : Doc =
        (if (vs.isEmpty)
             empty
         else
             line <>
             vsep (vs map toDoc)<>
             line) <>
        line <>
        vsep (ss map toDoc) <@>
        "return" <+> toDoc (r) <> semi

    override def toParenDoc (e : PrettyExpression) : Doc =
        positioned (e, toParenDocPositioned (e))

    def toParenDocPositioned (e : PrettyExpression) : Doc =
        e match {
            case IndExp (b, e) =>
                toDoc (b) <> brackets (toDoc (e))
            case LengthExp (e) =>
                toDoc (e) <> dot <> "length"
            case CallExp (b, i, as) =>
                toDoc (b) <> dot <> toDoc (i) <+> parens (hsep (as map toDoc, comma))
            case IntExp (v) =>
                value (v)
            case TrueExp () =>
                "true"
            case FalseExp () =>
                "false"
            case IdnExp (i) =>
                toDoc (i)
            case ThisExp () =>
                "this"
            case NewArrayExp (e) =>
                "new" <+> "int" <> brackets (toDoc (e))
            case NewExp (i) =>
                "new" <+> toDoc (i) <+> "()"
            case _ =>
                super.toParenDoc (e)
        }

}

/**
 * Abstract syntax tree pretty-printing for Minijava.
 */
object PrettyPrinter extends PrettyPrinter
