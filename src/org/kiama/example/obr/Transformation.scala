/**
 * Transformation of Obr language programs into SPARC machine code.
 *
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2010 Anthony M Sloane, Macquarie University.
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
package example.obr

/**
 * Module implementing transformation from Obr to SPARC assembler.
 */
object Transformation {

    import ObrTree._
    import SemanticAnalysis._
    import SPARCTree._
    import SymbolTable._
    import org.kiama.attribution.Attribution._

    /**
     * The SPARC machine program that is the translation of the given
     * Obr language program, comprising the translation of the program's
     * declarations and statements.  The bodies need to be computed first
     * so that the memory size is fully computed before we generate the
     * SPARC node.
     */
    val code : ObrInt ==> SPARC =
        attr {
            case ObrInt (_, decls, stmts, _) =>
                val dbody = decls.flatMap (ditems)
                val sbody = stmts.flatMap (sitems)
                SPARC (dbody ++ sbody, prevloc)
        }

    /**
     * Return the address for the location of the entity represented
     * by a given node.
     */
    private def location (n : EntityNode) : Address =
        n match {
            case IndexExp (_, i) =>
                Indexed (Local ((n->entity).locn), MulW (i->datum, IntDatum (WORDSIZE)))
            case FieldExp (_, f) =>
                val e @ Variable (RecordType (fs)) = n->entity
                Local (e.locn + fs.indexOf (f) * WORDSIZE)
            case _ =>
                Local ((n->entity).locn)
        }

    /**
     * The current label to which an EXIT statement should jump.  Undefined
     * to start with since there is no LOOP for an EXIT to appear inside.
     */
    private var exitlab : Label = _

    /**
     * The SPARC machine items that are the translation of the given
     * Obr language declaration.
     */
    private val ditems : Declaration ==> List[Item] =
        attr {

            /**
             * An integer parameter translates into a Read instruction into
             * the location of the parameter.
             */
            case d @ IntParam (_) =>
                 List (Read (location (d)))

            /**
             * All other kinds of declaration generate no code.
             */
            case _ =>
                 Nil

        }

    /**
     * The SPARC machine items that are the translation of the given
     * Obr language statement.
     */
    private val sitems : Statement ==> List[Item] =
        attr {

            /**
             * An assignment into an lvalue translates to a store of the rvalue
             * into the lvalue location.
             */
            case AssignStmt (e, exp) =>
                List (StW (location (e), exp->datum))

            /**
             * An EXIT statement translates into a jump to the exit label
             * of the current LOOP statement.
             */
            case ExitStmt () =>
                List (Jmp (exitlab))

            /**
             * A for statement first evaluates the minimum and maximum bounds,
             * storing one in the location of the loop control variable and
             * the other in a new temporary location.  Then the loop is ended
             * if the control variable is greater than the maximum value.
             * Otherwise, the body is executed.  After the body, if the control
             * variable is less than the maximum value, then we go back,
             * increment it and run the body again.  Note that this approach
             * avoids incrementing the control variable too far if the maximum
             * is the biggest integer that can be stored.
             */
            case e @ ForStmt (idn, min, max, body) =>
                val lab1 = genlabel
                val lab2 = genlabel
                val lab3 = genlabel
                val eloc = location (e)
                val maxloc = Local (Variable (IntType).locn)
                List (StW (eloc, min->datum),
                      StW (maxloc, max->datum),
                      Bne (CmpgtW (LdW (eloc), LdW (maxloc)), lab2),
                      Jmp (lab1),
                      LabelDef (lab3),
                      StW (eloc, AddW (LdW (eloc), IntDatum (1))),
                      LabelDef (lab1)) ++
                     body.flatMap (sitems) ++
                     List (Bne (CmpltW (LdW (eloc), LdW (maxloc)), lab3),
                           LabelDef (lab2))

            /**
             * A conditional statement translates into an evaluation of
             * the condition and branches to and around the appropriate
             * then and else parts.
             */
            case IfStmt (cond, thens, elses) =>
                val lab1 = genlabel
                val lab2 = genlabel
                List (Beq (cond->datum, lab1)) ++
                    thens.flatMap (sitems) ++
                    List (Jmp (lab2), LabelDef (lab1)) ++
                    elses.flatMap (sitems) ++
                    List (LabelDef (lab2))

            /**
             * A LOOP statement translates into a simple infinite loop
             * but we also have to keep track of the label to which an
             * EXIT statement should branch.  We need to save the label
             * on entry and restore it on exit in case this LOOP occurs
             * inside another one.
             */
            case LoopStmt (body) =>
                val lab1 = genlabel
                val lab2 = genlabel
                val savelab = exitlab
                exitlab = lab2
                val res = List (LabelDef (lab1)) ++
                    body.flatMap (sitems) ++
                    List (Jmp (lab1), LabelDef (lab2))
                exitlab = savelab
                res

            /**
             * A return statement translates into a Write instruction to
             * print the value being returned, then a Ret instruction to
             * terminate the program.
             */
            case ReturnStmt (exp) =>
                List (Write (exp->datum), Ret)

            /**
             * A while statement translates into the standard evaluation
             * of the condition and branching to the body.
             */
            case WhileStmt (cond, body) =>
                val lab1 = genlabel
                val lab2 = genlabel
                List (Jmp (lab1), LabelDef (lab2)) ++
                    body.flatMap (sitems) ++
                    List (LabelDef (lab1), Bne (cond->datum, lab2))

        }

    /**
     * The SPARC machine datum that is the translation of the given
     * Obr language expression.
     */
    private val datum : Expression ==> Datum =
        attr {

            /**
             * A short-circuited AND expression turns into a conditional
             * operation of the form if (l) r else 0.
             */
            case AndExp (l, r) =>
                Cond (l->datum, r->datum, IntDatum (0))

            /**
             * A true value translates into a one.
             */
            case BoolExp (true) =>
                IntDatum (1)

            /**
             * A true value translates into a zero.
             */
            case BoolExp (false) =>
                IntDatum (0)

            /**
             * An equality test turns into an equality operation.
             */
            case EqualExp (l, r) =>
                CmpeqW (l->datum, r->datum)

            /**
             * A field expression is a load from the location of the field.
             */
            case e @ FieldExp (_, _) =>
                LdW (location (e))

            /**
             * A greater than comparison turns into a greater than operation.
             */
            case GreaterExp (l, r) =>
                CmpgtW (l->datum, r->datum)

            /**
             * An identifier use translates to either a) an integer datum if
             * the identifier denotes a constant, or b) a loads of the value
             * from the location at which the variable is stored.
             */
            case e @ IdnExp (_) =>
                (e->entity) match {
                    case Constant (v) => IntDatum (v)
                    case _            => LdW (location (e))
                }

            /**
             * An index expression turns it a load from the location of the
             * array element that is being accessed.
             */
            case e @ IndexExp (_, _) =>
                LdW (location (e))

            /**
             * An integer expression translates directly to a datum that returns
             * that integer.
             */
            case IntExp (n) =>
                IntDatum (n)

            /**
             * A less than comparison turns into an less than operation.
             */
            case LessExp (l, r) =>
                CmpltW (l->datum, r->datum)

            /**
             * A minus expression turns into a subtraction operation.
             */
            case MinusExp (l, r) =>
                SubW (l->datum, r->datum)

            /**
             * A modulus expression turns into a remainder operation.
             */
            case ModExp (l, r) =>
                RemW (l->datum, r->datum)

            /**
             * A negation expression turns into a negation operation.
             */
            case NegExp (e) =>
                NegW (e->datum)

            /**
             * A Boolean complement expressions turns into a complement
             * operation.
             */
            case NotExp (e) =>
                Not (e->datum)

            /**
             * An inequality test turns into an inequality operation..
             */
            case NotEqualExp (l, r) =>
                CmpneW (l->datum, r->datum)

            /**
             * A short-circuited OR expression turns into a conditional
             * operation of the form if (l) 1 else r.
             */
            case OrExp (l, r) =>
                Cond (l->datum, IntDatum (1), r->datum)

            /**
             * A plus expression turns into an addition operation.
             */
            case PlusExp (l, r) =>
                AddW (l->datum, r->datum)

            /**
             * A slash expression turns into a division operation.
             */
            case SlashExp (l, r) =>
                DivW (l->datum, r->datum)

            /**
             * A star expression turns into a multiplication operation.
             */
            case StarExp (l, r) =>
                MulW (l->datum, r->datum)

        }

}
