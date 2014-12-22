/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2011-2014 Anthony M Sloane, Macquarie University.
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
package L3

trait NameAnalyser extends L2.NameAnalyser with SymbolTable {

    import base.source.{Block, IdnDef, IdnUse, ModuleDecl, SourceNode}
    import decorators.down
    import org.kiama.util.Entity
    import org.kiama.util.Messaging.{check, checkuse, message, Messages, noMessages}
    import source.{Call, FPSection, ProcDecl}

    /**
     * The error checking for this level.
     */
    override def errorsDef (n : SourceNode) : Messages =
        super.errorsDef (n) ++
        check (n) {
            case tree.parent.pair (u @ IdnUse (i1), ProcDecl (IdnDef (i2), _, _, _)) =>
                message (u, s"end procedure name $i1 should be $i2", i1 != i2)

            case u : IdnUse =>
                checkNonLocalVarAccess (u)

            case Call (u @ IdnUse (i), cps) =>
                checkuse (entity (u)) {
                    case _: BuiltinProc | _ : Procedure =>
                        noMessages
                    case _ =>
                        message (u, s"call of non-procedure $i")
                }
        }

    /**
     * Check for non-local variable and procedure accesses.  In the L3 language
     * non-local variable accesses are disallowed, unless they are to a variable
     * at the top level.  Non-local procedure accesses are just disallowed.
     */
    def checkNonLocalVarAccess (u : IdnUse) : Messages =
        checkuse (entity (u)) {
            case Procedure (i, p) =>
                message (u, s"non-local procedure access to $i is not allowed", level (u) > level (p))

            case Variable (i, t) if (level (t) != 0) && (level (u) > level (t)) =>
                message (u, s"non-local variable access to $i is not allowed")

            case Parameter (_, Variable (i, t)) if (level (t) != 0) && (level (u) > level (t)) =>
                message (u, s"non-local parameter access to $i is not allowed")
        }

    /**
     * Level of a node considering the module level to be zero and incrementing
     * each time we enter a nested procedure declaration.
     */
    lazy val level : SourceNode => Int =
        down[Int] (0) {
            case tree.parent.pair (n : ProcDecl, p) =>
                level (p) + 1
        }

    override def entityFromDecl (n : IdnDef, i : String) : Entity =
        n match {
            case tree.parent (p : ProcDecl) =>
                Procedure (i, p)
            case tree.parent (p : FPSection) =>
                Parameter (p.mode, Variable (i, p.tipe))
            case _ =>
                super.entityFromDecl (n, i)
        }

    /**
     * Blocks that are immediately inside procedure decls do not introduce new scopes
     * since the procedure itself does.  This computation overrides the one earlier
     * that pushes a scope for all blocks.  Don't include the procedure name in the
     * scope of its own body.
     */
    def envinl (in : SourceNode => Environment) : SourceNode ==> Environment = {
        case tree.parent.pair (b : Block, _ : ProcDecl) =>
            in (b)
    }

    override def envin (in : SourceNode => Environment) : SourceNode ==> Environment =
        (envinl (in)) orElse (super.envin (in))

    /**
     * Similarly for envin we don't need to leave a scope for a procedure block, since
     * we didn't enter one.  The IdnDef that is for a ProcDecl needs to be in the outer
     * scope, but not in the scope of its own body.  All arguments and local declarations
     * go in the nested scope.
     */
    def envoutl (out : SourceNode => Environment) : SourceNode ==> Environment = {
        case tree.parent.pair (b : Block, _ : ProcDecl) =>
            out (b)
        case tree.parent.pair (n @ IdnDef (i), p : ProcDecl) =>
            enter (define (out (n), i, entity (n)))
        case p : ProcDecl =>
            leave (out (p))
    }

    override def envout (out : SourceNode => Environment) : SourceNode ==> Environment =
        (envoutl (out)) orElse (super.envout (out))

}
