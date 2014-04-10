/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014 Anthony M Sloane, Macquarie University.
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
 * Abstract syntax tree pretty-printing for Miniava.
 */
object PrettyPrinter extends org.kiama.output.ParenPrettyPrinter {

    import MiniJavaTree._
    import org.kiama.output.PrettyExpression
    import scala.collection.immutable.Seq

    /**
     * Return a pretty-printed version of a node.
     */
    def pretty (t : MiniJavaTree) : String =
        super.pretty (show (t), 5)

    /**
     * Convert a PicoJava AST node to a pretty-printing document.
     */
    def show (t : MiniJavaTree) : Doc =
        t match {
            case Program (m, cs) =>
                show (m) <> ssep (cs map show, line <> line)
            case MainClass (i, s) =>
                "class" <+> show (i) <+> braces (
                    nest (
                        line <>
                        "public static void main ()" <+> braces (
                            nest (
                                line <>
                                show (s)
                            ) <>
                            line
                        )
                    ) <>
                    line
                ) <>
                line <>
                line
            case Class (i, optsc, b) =>
                val ext = optsc.map (n => "extends" <+> show (n)).getOrElse (empty)
                "class" <+> show (i) <> ext <+> braces (
                    nest (
                        line <>
                        show (b)
                    ) <>
                    line
                ) <>
                line <>
                line
            case ClassBody (fs, ms) =>
                vsep (fs map show) <@> vsep (ms map show)
            case Field (t, i) =>
                show (t) <+> show (i) <> semi
            case Var (t, i) =>
                show (t) <+> show (i) <> semi
            case Method (i, b) =>
                line <> "public" <+> show (b.tipe) <+> show (i) <+> show (b)
            case MethodBody (_, as, vs, ss, r) =>
                parens (hsep (as map show, comma)) <>
                    line <>
                    braces (
                        nest (showbody (vs, ss, r)) <>
                        line
                    )
            case Argument (t, i) =>
                show (t) <+> show (i)
            case t : Type =>
                t.toString ()
            case Block (ss) =>
                braces (
                    nest (
                        line <>
                        vsep (ss map show)
                    ) <>
                    line
                )
            case If (e, s1, s2) =>
                "if" <+> parens (show (e)) <>
                    nest (
                        line <>
                        show (s1)
                    ) <>
                    line <>
                    "else" <>
                    nest (
                        line <>
                        show (s2)
                    )
            case While (e, s) =>
                "while" <+> parens (show (e)) <+> show (s)
            case Println (e) =>
                "System.out.println" <+> parens (show (e)) <> semi
            case VarAssign (i, e) =>
                show (i) <+> equal <+> show (e) <> semi
            case ArrayAssign (i, e1, e2) =>
                show (i) <> brackets (show (e1)) <+> equal <+> show (e2) <> semi
            case n : IdnTree =>
                n.idn
            case e : PrettyExpression =>
                toParenDoc (e)
        }

    def showbody (vs : Seq[Var], ss : Seq[Statement], r : Expression) : Doc =
        (if (vs.isEmpty)
             empty
         else
             line <>
             vsep (vs map show)<>
             line) <>
        line <>
        vsep (ss map show) <@>
        "return" <+> show (r) <> semi

    override def toParenDoc (e : PrettyExpression) : Doc =
        e match {
            case IndExp (b, e) =>
                show (b) <> brackets (show (e))
            case LengthExp (e) =>
                show (e) <> dot <> "length"
            case CallExp (b, i, as) =>
                show (b) <> dot <> show (i) <+> parens (hsep (as map show, comma))
            case IntExp (v) =>
                value (v)
            case TrueExp () =>
                "true"
            case FalseExp () =>
                "false"
            case IdnExp (i) =>
                show (i)
            case ThisExp () =>
                "this"
            case NewArrayExp (e) =>
                "new" <+> "int" <> brackets (show (e))
            case NewExp (i) =>
                "new" <+> show (i) <+> "()"
            case _ =>
                super.toParenDoc (e)
        }

}
