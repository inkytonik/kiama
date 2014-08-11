/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2014 Anthony M Sloane, Macquarie University.
 * Copyright (C) 2010-2014 Dominic Verity, Macquarie University.
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
 * Module implementing transformation from Obr to RISC tree code.
 */
class RISCTransformer (analyser : SemanticAnalyser, labels : RISCLabels) {

    import analyser.{divideByZeroExn, entity, indexOutOfBoundsExn}
    import labels.genlabelnum
    import ObrTree._
    import RISCTree._
    import SymbolTable._
    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators
    import org.kiama.util.{Counter, Entity}
    import scala.collection.immutable.Seq

    val tree = analyser.tree
    val decorators = new Decorators (tree)
    import decorators.down

    /**
     * The RISC machine program that is the translation of the given
     * Obr language program, comprising the translation of the program's
     * declarations and statements.  The bodies need to be computed first
     * so that the memory size is fully computed before we generate the
     * RISC node.
     */
    val code : ObrInt => RISCProg =
        attr {
            case p @ ObrInt (_, decls, stmts, _) =>
                val dbody = decls.flatMap (ditems)
                val sbody = stmts.flatMap (sitems)
                val sepilogue = Seq (
                        Write (IntDatum (0))
                    ,   Ret ()
                    ,   LabelDef (Label (exnlab (p)))
                    ,   Write (IntDatum (-1))
                    )
                RISCProg (dbody ++ sbody ++ sepilogue)
        }

    /**
     * Counter for previously used location.
     */
    val prevLocCounter = new Counter (0)

    /**
     * Get the location of an entity. Only valid for variables,
     * but defined on all entities.
     */
    val locn : Entity => Int =
        attr {
            case entity @ Variable (tipe) =>
                val loc = prevLocCounter.value
                prevLocCounter.next (tipe.storage)
                loc
            case _ =>
                0
        }

    /**
     * Return the address for the location of the entity represented
     * by a given node.
     */
    def location (n : EntityTree) : Address =
        n match {
            case e @ IndexExp (v, i) =>
                val lab1 = genlabelnum ()
                val lab2 = genlabelnum ()
                Indexed (
                    Local (locn (entity (v))),
                    MulW (
                        SequenceDatum(
                            Seq(
                                StW (Local (tempintloc), SubW (datum (i), IntDatum(1))),
                                Bne (CmpltW (LdW (Local (tempintloc)), IntDatum (0)), Label (lab1)),
                                Beq (CmpltW (
                                    entity (v) match {
                                        case Variable (ArrayType (size)) => IntDatum (size-1)
                                    }, LdW (Local (tempintloc))), Label (lab2)),
                                LabelDef (Label (lab1)),
                                StW (Local (exnloc), IntDatum (indexOutOfBoundsExn)),
                                Jmp (Label (exnlab (e))),
                                LabelDef (Label (lab2))),
                            LdW (Local (tempintloc))),
                        IntDatum (WORDSIZE)))
            case FieldExp (v, f) =>
                val e @ Variable (RecordType (fs)) = entity (v)
                Local (locn (e) + fs.indexOf (f) * WORDSIZE)
            case _ =>
                Local (locn (entity (n.idn)))
        }

    /**
     * The current label to which an EXIT statement should jump when used
     * at the given context.
     */
    val exitlab =
        down[Int] (0) {
            case _ : LoopStmt =>
                genlabelnum ()
        }

    /**
     * The label marking the entry point to the currently active error handler
     * (CATCH block). Defaults to a global handler outside of all TRY...CATCH blocks
     *
     * Notice that since this version of Obr has no procedures, the currently
     * active exception handler can be determined completely statically.
     */
    val exnlab =
        down[Int] (0) {

            // Programs and Try statements can catch exceptions.
            case _ : ObrInt | _ : TryStmt =>
                genlabelnum ()

            // Catch blocks don't use the exnlab of their context, but the
            // one from the context of the Try in which they occur, since
            // re-raising an exception has to go out one level.
            case s : Catch =>
                exnlabOuter (s)

        }

    /**
     * The exception label for the context outside the current Try statement.
     * Only valid when used inside a Try statement.
     */
    val exnlabOuter : ObrNode => Int =
        down[Int] (0) {
            case tree.parent.pair (s : TryStmt, p) =>
                exnlab (p)
        }

    /**
     * A location reserved for storing temporary integer values while
     * they are checked for possible division by zero or index out of
     * bounds errors.
     */
    val tempintloc : Int =
        locn (Variable (IntType ()))

    /**
     * A location reserved for storing the exception value associated with
     * a raised exception.
     */
    val exnloc : Int =
        locn (Variable (ExnType ()))

    /**
     * The RISC tree items that are the translation of the given
     * Obr language declaration.
     */
    val ditems : Declaration => Seq[Item] =
        attr {

            /**
             * An integer parameter translates into a Read instruction into
             * the location of the parameter.
             */
            case d @ IntParam (_) =>
                 Seq (StW (location (d), Read ()))

            /**
             * All other kinds of declaration generate no code.
             */
            case _ =>
                 Nil

        }

    /**
     * The RISC tree items that are the translation of the given
     * Obr language statement.
     */
    val sitems : Statement => Seq[Item] =
        attr {

            /**
             * An assignment into an lvalue translates to a store of the rvalue
             * into the lvalue location.
             */
            case AssignStmt (e, exp) =>
                Seq (StW (location (e), datum (exp)))

            /**
             * An EXIT statement translates into a jump to the exit label
             * of the current LOOP statement.
             */
            case s @ ExitStmt () =>
                Seq (Jmp (Label (exitlab (s))))

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
                val lab1 = genlabelnum ()
                val lab2 = genlabelnum ()
                val lab3 = genlabelnum ()
                def eloc = location (e)
                // store current value of first unallocated location
                val origprevloc = prevLocCounter.value
                // allocate a temporary location to store the calculated
                // maximum index value for this for loop.
                val maxloc = locn (Variable (IntType ()))
                // generate RISCTree code
                val result =
                    Seq (StW (eloc, datum (min)),
                         StW (Local (maxloc), datum (max)),
                         Bne (CmpgtW (LdW (eloc), LdW (Local (maxloc))), Label (lab2)),
                         Jmp (Label (lab1)),
                         LabelDef (Label (lab3)),
                         StW (eloc, AddW (LdW (eloc), IntDatum (1))),
                         LabelDef (Label (lab1))) ++
                    body.flatMap (sitems) ++
                    Seq (Bne (CmpltW (LdW (eloc), LdW (Local (maxloc))), Label (lab3)),
                          LabelDef (Label (lab2)))
                // restore value of first unallocated location
                // thereby deallocating our temporary.
                prevLocCounter.reset (origprevloc)
                // ... and return our generated RISCTree code
                result

            /**
             * A conditional statement translates into an evaluation of
             * the condition and branches to and around the appropriate
             * then and else parts.
             */
            case IfStmt (cond, thens, elses) =>
                val lab1 = genlabelnum ()
                val lab2 = genlabelnum ()
                Seq (Beq (datum (cond), Label (lab1))) ++
                    thens.flatMap (sitems) ++
                    Seq (Jmp (Label (lab2)), LabelDef (Label (lab1))) ++
                    elses.flatMap (sitems) ++
                    Seq (LabelDef (Label (lab2)))

            /**
             * A LOOP statement translates into a simple infinite loop
             * but we also have to keep track of the label to which an
             * EXIT statement should branch.  We need to save the label
             * on entry and restore it on exit in case this LOOP occurs
             * inside another one.
             */
            case s @ LoopStmt (body) =>
                // Generate label for top of loop body
                val lab1 = genlabelnum ()
                // Construct loop code
                Seq (LabelDef (Label (lab1))) ++
                    body.flatMap (sitems) ++
                    Seq (Jmp (Label (lab1)),
                    LabelDef (Label (exitlab (s))))

            /**
             * A return statement translates into a Write instruction to
             * print the value being returned, then a Ret instruction to
             * terminate the program.
             */
            case ReturnStmt (exp) =>
                Seq (Write (datum (exp)), Ret ())

            /**
             * A while statement translates into the standard evaluation
             * of the condition and branching to the body.
             */
            case WhileStmt (cond, body) =>
                val lab1 = genlabelnum ()
                val lab2 = genlabelnum ()
                Seq (Jmp (Label (lab1)), LabelDef (Label (lab2))) ++
                    body.flatMap (sitems) ++
                    Seq (LabelDef (Label (lab1)), Bne (datum (cond), Label (lab2)))

            /**
             * A TRY statement translates to the code generated from its body followed by
             * the code generated from each of its catch blocks. Between these we must also
             * provide a jump instruction to jump around the catch blocks, avoiding drop
             * through, should execution of the body complete without raising an exception.
             *
             * The label stored in exnlab is used to translate any RAISE statements, which
             * are essentially just jumps to that label. But we need to be careful, because
             * this label should be set to point to the entry point of the CATCH block of
             * the current TRY statement within its body and to the exception handler which
             * is associated with the enclosing scope within the bodies of its CATCH blocks.
             */
            case s @ TryStmt (TryBody (body), cblocks) =>
                // A label for the exit point of the exception handler code
                val tryexitlab = genlabelnum ()
                // Construct the translated RISC code for the body of this try statement
                val tbody = body.flatMap (sitems)
                // Translate the catch clauses, append the resulting RISC code to that for
                // the body and return
                tbody ++ Seq (Jmp (Label (tryexitlab)), LabelDef (Label (exnlab (s)))) ++
                    cblocks.flatMap (cblock (_, tryexitlab)) ++
                    Seq (Jmp (Label (exnlabOuter (s))), LabelDef (Label (tryexitlab)))

            /**
             * A RAISE statement stores the integer associated with the specified
             * exception into a memory location reserved for the current exception
             * value and jumps to the exception handler (CATCH blocks) for the
             * current scope.
             */
            case RaiseStmt (s) =>
                // Get hold of the integer constant associated with the exception
                // kind specified in this RAISE statement
                val exnconst = entity (s) match {
                    case Constant (_, v) => IntDatum (v)
                }
                // Generate code to store that vale as the current exception number
                // and then to jump to the entry point of the exception handler currently
                // in scope.
                Seq (StW (Local (exnloc), exnconst), Jmp (Label (exnlab (s))))

        }

    /**
     * Translate a catch block into RISC machine code. Each one simply translates
     * to a test of the value of the current exception followed by code which
     * executed if that test succeeds.
     */
    def cblock (clause : Catch, exitlab : Int) : Seq[Item] =
        clause match {

            case Catch (n, stmts) =>
                // Generate a label for the exit point of this catch block.
                val lab1 = genlabelnum ()
                // Get hold of the integer constant associated with the exception
                // identifier of this catch block from the entity of this node.
                val exnconst = entity (n) match {
                    case Constant (_, v) => IntDatum (v)
                }
                // Generate code for statements in the catch body, guarded by a
                // conditional branch to test the exception value thrown.
                Seq (Beq (CmpeqW (LdW (Local (exnloc)), exnconst), Label (lab1))) ++
                    stmts.flatMap (sitems) ++
                    Seq (Jmp (Label (exitlab)), LabelDef (Label (lab1)))

        }

    /**
     * The RISC machine datum that is the translation of the given
     * Obr language expression.
     */
    val datum : Expression => Datum =
        attr {

            /**
             * A short-circuited AND expression turns into a conditional
             * operation of the form if (l) r else 0.
             */
            case AndExp (l, r) =>
                Cond (datum (l), datum (r), IntDatum (0))

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
                CmpeqW (datum (l), datum (r))

            /**
             * A field expression is a load from the location of the field.
             */
            case e @ FieldExp (_, _) =>
                LdW (location (e))

            /**
             * A greater than comparison turns into a greater than operation.
             */
            case GreaterExp (l, r) =>
                CmpgtW (datum (l), datum (r))

            /**
             * An identifier use translates to either a) an integer datum if
             * the identifier denotes a constant, or b) a loads of the value
             * from the location at which the variable is stored.
             */
            case e @ IdnExp (n) =>
                entity (n) match {
                    case Constant (_, v) => IntDatum (v)
                    case _               => LdW (location (e))
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
                CmpltW (datum (l), datum (r))

            /**
             * A minus expression turns into a subtraction operation.
             */
            case MinusExp (l, r) =>
                SubW (datum (l), datum (r))

            /**
             * A modulus expression turns into a remainder operation.
             * See the translation of SlashExps for more information.
             */
            case e @ ModExp (l, r) =>
                val lab1 = genlabelnum ()
                SequenceDatum (
                    Seq (
                            StW (Local (tempintloc), datum (r))
                        ,   Bne (LdW (Local (tempintloc)), Label (lab1))
                        ,   StW (Local (exnloc), IntDatum (divideByZeroExn))
                        ,   Jmp (Label (exnlab (e)))
                        ,   LabelDef (Label (lab1))
                        )
                ,   RemW (datum (l), LdW (Local (tempintloc)))
                )

            /**
             * A negation expression turns into a negation operation.
             */
            case NegExp (e) =>
                NegW (datum (e))

            /**
             * A Boolean complement expressions turns into a complement
             * operation.
             */
            case NotExp (e) =>
                Not (datum (e))

            /**
             * An inequality test turns into an inequality operation..
             */
            case NotEqualExp (l, r) =>
                CmpneW (datum (l), datum (r))

            /**
             * A short-circuited OR expression turns into a conditional
             * operation of the form if (l) 1 else r.
             */
            case OrExp (l, r) =>
                Cond (datum (l), IntDatum (1), datum (r))

            /**
             * A plus expression turns into an addition operation.
             */
            case PlusExp (l, r) =>
                AddW (datum (l), datum (r))

            /**
             * A slash expression turns into a division operation.
             * However, we also need to check the second operand to
             * see if it is 0 and if it is we raise a DivideByZero
             * exception.
             */
            case e @ SlashExp (l, r) =>
                val lab1 = genlabelnum ()
                SequenceDatum (
                        Seq (
                                // Calculate value of right operand and store result
                                // in the memory location reserved for temporaries.
                                StW (Local (tempintloc), datum (r))
                                // If this value is non-zero then branch to the code
                                // which actually calculates this division operation.
                            ,   Bne (LdW (Local (tempintloc)), Label (lab1))
                                // Store the integer associated with DivideByZero exceptions
                                // into the location reserved for the current exn number.
                            ,   StW (Local (exnloc), IntDatum (divideByZeroExn))
                                // Raise this exception by jumping to the current
                                // (nearest enclosing) exception handler.
                            ,   Jmp (Label (exnlab (e)))
                            ,   LabelDef (Label (lab1))
                            )
                        // Finally execute the division operation.
                    ,   DivW (datum (l), LdW (Local (tempintloc)))
                    )

            /**
             * A star expression turns into a multiplication operation.
             */
            case StarExp (l, r) =>
                MulW (datum (l), datum (r))

        }

}
