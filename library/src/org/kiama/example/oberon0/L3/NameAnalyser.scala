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
package L3

trait NameAnalyser extends L2.NameAnalyser with SymbolTable {

    import base.source.{Block, IdnDef, IdnUse, ModuleDecl, SourceTree}
    import messaging.message
    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators.down
    import org.kiama.util.Patterns.HasParent
    import source.{Call, FPSection, ProcDecl}

    abstract override def check (n : SourceTree) {
        n match {
            case HasParent (u @ IdnUse (i1), ProcDecl (IdnDef (i2), _, _, _)) =>
                if (i1 != i2)
                    message (u, s"end procedure name $i1 should be $i2")

            case u : IdnUse =>
                checkNonLocalVarAccess (u)

            case Call (u @ IdnUse (i), cps) =>
                (u->entity) match {
                    case _: BuiltinProc | _ : Procedure =>
                        // Ok
                    case _ =>
                        message (n, s"call of non-procedure $i")
                }

            case _ =>
                // Do nothing by default
        }

        super.check (n)
    }

    /**
     * Check for non-local variable and procedure accesses.  In the L3 language
     * non-local variable accesses are disallowed, unless they are to a variable
     * at the top level.  Non-local procedure accesses are just disallowed.
     */
    def checkNonLocalVarAccess (u : IdnUse) {
        (u->entity) match {
            case Procedure (i, p) =>
                if (u->level > p->level)
                    message (u, s"non-local procedure access to $i is not allowed")

            case Variable (i, t) if (t->level != 0) && (u->level > t->level) =>
                message (u, s"non-local variable access to $i is not allowed")

            case Parameter (_, Variable (i, t)) if (t->level != 0) && (u->level > t->level) =>
                message (u, s"non-local parameter access to $i is not allowed")

            case _ =>
                // Ok
        }
    }

    /**
     * Level of a node considering the module level to be zero and incrementing
     * each time we enter a nested procedure declaration.
     */
    lazy val level : SourceTree => Int =
        down[SourceTree, Int] {
            case _ : ModuleDecl => 0
            case n : ProcDecl   => (n.parent[SourceTree]->level) + 1
        }

    override def entityFromDecl (n : IdnDef, i : String) : Entity =
        n.parent match {
            case p : ProcDecl  => Procedure (i, p)
            case p : FPSection => Parameter (p.mode, Variable (i, p.tipe))
            case _             => super.entityFromDecl (n, i)
        }

    /**
     * Blocks that are immediately inside procedure decls do not introduce new scopes
     * since the procedure itself does.  This computation overrides the one earlier
     * that pushes a scope for all blocks.  Don't include the procedure name in the
     * scope of its own body.
     */
    def envinl (in : SourceTree => Environment) : SourceTree ==> Environment = {
        case HasParent (b : Block, _ : ProcDecl) =>
            in (b)
    }

    override def envin (in : SourceTree => Environment) : SourceTree ==> Environment =
        (envinl (in)) orElse (super.envin (in))

    /**
     * Similarly for envin we don't need to leave a scope for a procedure block, since
     * we didn't enter one.  The IdnDef that is for a ProcDecl needs to be in the outer
     * scope, but not in the scope of its own body.  All arguments and local declarations
     * go in the nested scope.
     */
    def envoutl (out : SourceTree => Environment) : SourceTree ==> Environment = {
        case HasParent (b : Block, _ : ProcDecl) =>
            out (b)
        case HasParent (n @ IdnDef (i), p : ProcDecl) =>
            enter (define (n->out, i, n->entity))
        case p : ProcDecl =>
            leave (out (p))
    }

    override def envout (out : SourceTree => Environment) : SourceTree ==> Environment =
        (envoutl (out)) orElse (super.envout (out))

}
