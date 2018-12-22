/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2018 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.oberon0
package L3

trait NameAnalyser extends L2.NameAnalyser with SymbolTable {

    import base.source.{Block, IdnDef, IdnUse, SourceNode}
    import decorators.down
    import org.bitbucket.inkytonik.kiama.util.Entity
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, checkUse, error, Messages, noMessages}
    import source.{Call, FPSection, ProcDecl}

    /**
     * The error checking for this level.
     */
    override def errorsDef(n : SourceNode) : Messages =
        super.errorsDef(n) ++
            check(n) {
                case tree.parent.pair(u @ IdnUse(i1), ProcDecl(IdnDef(i2), _, _, _)) =>
                    error(u, s"end procedure name $i1 should be $i2", i1 != i2)

                case u : IdnUse =>
                    checkNonLocalVarAccess(u)

                case Call(u @ IdnUse(i), cps) =>
                    checkUse(entity(u)) {
                        case _ : BuiltinProc | _ : Procedure =>
                            noMessages
                        case _ =>
                            error(u, s"call of non-procedure $i")
                    }
            }

    /**
     * Check for non-local variable and procedure accesses.  In the L3 language
     * non-local variable accesses are disallowed, unless they are to a variable
     * at the top level.  Non-local procedure accesses are just disallowed.
     */
    def checkNonLocalVarAccess(u : IdnUse) : Messages =
        checkUse(entity(u)) {
            case Procedure(i, p) =>
                error(u, s"non-local procedure access to $i is not allowed", level(u) > level(p))

            case Variable(i, t) if (level(t) != 0) && (level(u) > level(t)) =>
                error(u, s"non-local variable access to $i is not allowed")

            case Parameter(_, Variable(i, t)) if (level(t) != 0) && (level(u) > level(t)) =>
                error(u, s"non-local parameter access to $i is not allowed")
        }

    /**
     * Level of a node considering the module level to be zero and incrementing
     * each time we enter a nested procedure declaration.
     */
    lazy val level : SourceNode => Int =
        down[Int](0) {
            case tree.parent.pair(n : ProcDecl, p) =>
                level(p) + 1
        }

    override def entityFromDecl(n : IdnDef, i : String) : Entity =
        n match {
            case tree.parent(p : ProcDecl) =>
                Procedure(i, p)
            case tree.parent(p : FPSection) =>
                Parameter(p.mode, Variable(i, p.tipe))
            case _ =>
                super.entityFromDecl(n, i)
        }

    /**
     * Blocks that are immediately inside procedure decls do not introduce new scopes
     * since the procedure itself does.  This computation overrides the one earlier
     * that pushes a scope for all blocks.  Don't include the procedure name in the
     * scope of its own body.
     */
    def envinl(in : SourceNode => Environment) : SourceNode ==> Environment = {
        case tree.parent.pair(b : Block, _ : ProcDecl) =>
            in(b)
    }

    override def envin(in : SourceNode => Environment) : SourceNode ==> Environment =
        (envinl(in)) orElse (super.envin(in))

    /**
     * Similarly for envin we don't need to leave a scope for a procedure block, since
     * we didn't enter one.  The IdnDef that is for a ProcDecl needs to be in the outer
     * scope, but not in the scope of its own body.  All arguments and local declarations
     * go in the nested scope.
     */
    def envoutl(out : SourceNode => Environment) : SourceNode ==> Environment = {
        case tree.parent.pair(b : Block, _ : ProcDecl) =>
            out(b)
        case tree.parent.pair(n @ IdnDef(i), p : ProcDecl) =>
            enter(define(out(n), i, entity(n)))
        case p : ProcDecl =>
            leave(out(p))
    }

    override def envout(out : SourceNode => Environment) : SourceNode ==> Environment =
        (envoutl(out)) orElse (super.envout(out))

}
