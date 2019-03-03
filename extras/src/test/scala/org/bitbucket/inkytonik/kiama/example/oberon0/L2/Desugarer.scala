/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L2

/**
 * Desugaring transformation for L2.
 */
trait Desugarer extends L0.Desugarer {

    import base.source.{
        Block,
        Expression,
        IdnDef,
        IdnUse,
        Statement
    }
    import base.source.SourceTree.SourceTree
    import L0.source.{
        AddExp,
        AndExp,
        Assignment,
        EqExp,
        GeExp,
        IdnExp,
        IntExp,
        LeExp,
        NamedType,
        OrExp,
        VarDecl
    }
    import L1.source.{IfStatement, WhileStatement}
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywhere, rewriteTree, rule}
    import source.{
        Case,
        CaseStatement,
        Condition,
        ForStatement,
        MinMaxCond,
        ValCond
    }

    /**
     * Desugar FOR and CASE statements into simpler constructs.
     * Then call the next level of transformation.
     */
    override def transform(tree : SourceTree) : SourceTree = {

        /*
         * An analyser for the input tree.
         */
        val analyser = buildAnalyser(tree)
        import analyser.value

        /*
         * Desugar FOR statements into equivalent blocks containing a WHILE loop.
         * A new variable called "limit" is introduced in the block to hold the
         * upper limit of the FOR to protect against changes in the body.
         * Specifically,
         *   FOR id := e1 TO e2 BY e3 DO body END
         * is transformed into
         *   VAR
         *     limit : INTEGER;
         *   BEGIN
         *     id := e1;
         *     limit := e2;
         *     WHILE (id op limit) DO
         *       body
         *       id := id + e3
         *     END
         *   END
         * If e3 is negative, op is <=, otherwise it is >=.
         */
        lazy val desugarFor =
            rule[Statement] {
                case ForStatement(idnexp, lower, upper, optby, Block(Vector(), stmts)) =>
                    val limvarname = "_limit"
                    val limexp = IdnExp(IdnUse(limvarname))
                    val incval = optby.map(value).getOrElse(1)
                    val rincval = IntExp(incval)
                    val cond = if (incval >= 0)
                        LeExp(idnexp, limexp)
                    else
                        GeExp(idnexp, limexp)
                    Block(
                        Vector(
                            VarDecl(
                                Vector(IdnDef(limvarname)),
                                NamedType(IdnUse("INTEGER"))
                            )
                        ),
                        Vector(
                            Assignment(idnexp, lower),
                            Assignment(limexp, upper),
                            WhileStatement(
                                cond,
                                Block(
                                    Vector(),
                                    stmts :+
                                        Assignment(idnexp, AddExp(idnexp, rincval))
                                )
                            )
                        )
                    )
            }

        /*
         * Desugar CASE statements into equivalent blocks containing cascading IF
         * statements. A new variable called "casevar" is introduced in the block to
         * hold the selection value so that it does not need to be re-evaluated.
         * Specifically,
         *   CASE e OF
         *     cases
         *   END
         * is transformed into
         *   VAR
         *     caseval : INTEGER;
         *   BEGIN
         *     caseval := e;
         *     IF caseval ... THEN
         *       ...
         *     END
         *   END
         */
        lazy val desugarCase =
            rule[Statement] {
                case CaseStatement(exp, cases, optelse) =>
                    val casevarname = "_caseval"
                    val caseexp = IdnExp(IdnUse(casevarname))
                    Block(
                        Vector(
                            VarDecl(
                                Vector(IdnDef(casevarname)),
                                NamedType(IdnUse("INTEGER"))
                            )
                        ),
                        Vector(
                            Assignment(caseexp, exp),
                            casesToIf(caseexp, cases, optelse)
                        )
                    )
            }

        /*
         * Return an IF cascade equivaleant to the given cases and optional else
         * block. The variable ce holds the selection value.
         * Specifically, these cases on ce
         *   CASE e1       : s1
         *   CASE e2 .. e3 : s2
         *   ELSE s3
         * are transformed into
         *   IF ce = e1 THEN
         *     s1
         *   ELSEIF (ce >= e2) & (ce <= e3) THEN
         *     s2
         *   ELSE
         *     32
         *   END
         * If a case has more than one condition then they are combined with Or
         * operators.
         */
        def casesToIf(ce : IdnExp, cases : Vector[Case], optelse : Option[Block]) : IfStatement = {

            /*
             * Return an expression for a case condition. A value condition becomes
             * a simple equality and a min-max condition becomes a range test.
             */
            def condToExp(n : Condition) : Expression =
                n match {
                    case ValCond(e) =>
                        EqExp(ce, e)
                    case MinMaxCond(e1, e2) =>
                        AndExp(GeExp(ce, e1), LeExp(ce, e2))
                }

            /*
             * Return a single expression for a sequence of conditions by
             * forming a disjunction of translating each one.
             */
            def condsToExp(ns : Vector[Condition]) : Expression = {
                val es = ns.map(condToExp)
                es.tail.foldLeft(es.head)(OrExp)
            }

            // Extract the first (f) and rest (tl) cases.
            val f +: tl = cases

            // Produce an IF which implements the first case in the THEN branch,
            // the other cases as the ELSE-IFs, and the optional ELSE.
            IfStatement(
                condsToExp(f.conds),
                f.block,
                tl.map {
                    case Case(es, b) => (condsToExp(es), b)
                },
                optelse
            )

        }

        val desugarer = everywhere(desugarFor + desugarCase)
        super.transform(rewriteTree(desugarer)(tree))

    }

}
