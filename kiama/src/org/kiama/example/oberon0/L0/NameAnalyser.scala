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
package L0

trait NameAnalyser extends base.Analyser with SymbolTable {

    import base.source.{Block, Identifier, IdnDef, IdnUse, ModuleDecl,
        SourceASTNode}
    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators.{chain, Chain}
    import org.kiama.util.Messaging.message
    import org.kiama.util.Patterns.HasParent
    import source.{AddExp, Assignment, BinaryExpression, ConstDecl,
        DivExp, Expression, IdnExp, IntExp, ModExp, MulExp, NamedType, NegExp,
        SubExp, TypeDecl, TypeDef, UnaryExpression, VarDecl}

    abstract override def check (n : SourceASTNode) {
        n match {
            case ModuleDecl (_, _, u @ IdnUse (i)) if !isModule (u->entity) =>
                message (u, s"$i is not a module")

            case d @ IdnDef (i) if d->entity == MultipleEntity () =>
                message (d, s"$i is already declared")

            case HasParent (u @ IdnUse (i2), ModuleDecl (IdnDef (i1), _, _)) if i1 != i2 =>
                message (u, s"end module name '$i2' should be '$i1'")

            case u @ IdnUse (i) if u->entity == UnknownEntity () =>
                message (u, s"$i is not declared")

            case NamedType (u @ IdnUse (i)) if !isType (u->entity) =>
                message (u, s"$i is not a type name")

            case Assignment (l, _) if !isLvalue (l) =>
                message (n, "illegal assignment")

            case e : Expression =>
                if ((e->rootconstexp) && !(e->isconst))
                    message (e, "expression is not constant")
                e match {
                    case u @ IdnExp (IdnUse (i)) if !(isRvalue (u)) =>
                        message (u, s"$i cannot be used in an expression")

                    case DivExp (_, r) if (r->expconst) && (r->isconst) && (r->value == 0) =>
                        message (r, "division by zero in constant expression")

                    case ModExp (_, r) if (r->expconst) && (r->isconst) && (r->value == 0) =>
                        message (r, "modulus by zero in constant expression")

                    case _ =>
                        // Do nothing by default
                }

            case _ =>
                // Do nothing by default
        }

        super.check (n)
    }

    /**
     * Return true if the expression can legally appear on the left-hand side of an
     * assignment statement.  At this level only allow identifiers of variables or
     * things we don't know anything about.  The true default is used so that this
     * computation can be used in redefinitions.
     */
    def isLvalue (l : Expression) : Boolean =
        l match {
            case IdnExp (u @ IdnUse (i)) =>
                isVariable (u->entity)
            case _ =>
                false
        }

    /**
     * Return true if the identifier is an r-value and hence its value can be
     * used (ie. it's erroneous or is a constant, value or variable).
     */
    def isRvalue (r : IdnExp) : Boolean = {
        val e = (r.idnuse)->entity
        isLvalue (r) || e.isInstanceOf[Constant] || e.isInstanceOf[IntegerValue]
    }

    /**
     * The entity for an identifier definition as given by its declaration
     * context.
     */
    def entityFromDecl (n : IdnDef, i : String) : Entity =
        n.parent match {
            case p : ModuleDecl => Module (i, p)
            case p : ConstDecl  => Constant (i, p)
            case p : TypeDecl   => UserType (i, p)
            case p : VarDecl    => Variable (i, p.tipe)
        }

    /**
     * The program entity referred to by an identifier definition or use.  In
     * the case of a definition it's the thing being defined, so define it to
     * be a reference to the declaration.  If it's already defined, return a
     * entity that indicates a multiple definition.  In the case of a use,
     * it's the thing defined elsewhere that is being referred to here, so
     * look it up in the environment.
     */
    lazy val entity : Identifier => Entity =
        attr {
            case n @ IdnDef (i) =>
                if (isDefinedInScope (n->(env.in), i))
                    MultipleEntity ()
                else
                    entityFromDecl (n, i)
            case n @ IdnUse (i) =>
                lookup (n->(env.in), i, UnknownEntity ())
        }

    /**
     * The environment containing bindings for all identifiers visible at the
     * given node.  It starts at the module declaration with the default
     * environment.  At blocks we enter a nested scope which is removed on
     * exit from the block.  At constant and type declarations the left-hand
     * side binding is not in scope on the right-hand side.  Each identifier
     * definition just adds its binding to the chain.  The envout cases for
     * assignment and expression mean that we don't need to traverse into
     * those constructs, since declarations can't occur there.
     */
    lazy val env : Chain[SourceASTNode,Environment] =
        chain (envin, envout)

    def envin (in : SourceASTNode => Environment) : SourceASTNode ==> Environment = {
        case _ : ModuleDecl                            => enter (defenv)
        case b : Block                                 => enter (in (b))
        case HasParent (_ : Expression, p : ConstDecl) => p->(env.in)
        case HasParent (_ : TypeDef, p : TypeDecl)     => p->(env.in)
    }

    def envout (out : SourceASTNode => Environment) : SourceASTNode ==> Environment = {
        case b : Block        => leave (out (b))
        case ConstDecl (d, _) => d->env
        case TypeDecl (d, _)  => d->env
        case n @ IdnDef (i)   => define (n->out, i, n->entity)
        case a : Assignment   => a->(env.in)
        case e : Expression   => e->(env.in)
    }

    /**
     * Is this expression the root of what is expected to be a constant
     * expression? At this level only expressions on the RHS of a constant
     * declaration have this property.
     */
    lazy val rootconstexp : Expression => Boolean =
        attr (rootconstexpDef)

    def rootconstexpDef : Expression => Boolean =
        (e =>
            (e.parent) match {
                case _ : ConstDecl  => true
                case _              => false
            })

    /**
     * Is an expression expected to be constant or not? Either the expression
     * is the root of an expected constant expression or its parent expression
     * is expected to be constant.
     */
    lazy val expconst : Expression => Boolean =
        attr {
            case e if e->rootconstexp =>
                true
            case HasParent (e, p : Expression) =>
                p->expconst
            case _ =>
                false
        }

    /**
     * Is an expression constant or not?  Unknown entities are constant.
     * Strictly speaking we only need to support integer expressions here,
     * but we treat Boolean ones as constant in the same way so that we
     * avoid spurious errors.  Type analysis will reject Boolean constant
     * expressions anyway.
     */
    lazy val isconst : Expression => Boolean =
        attr {
            case IdnExp (n) if isConstant (n->entity) =>
                true
            case IntExp (n) =>
                true
            case e : UnaryExpression =>
                (e.exp)->isconst
            case e : BinaryExpression =>
                ((e.left)->isconst) && ((e.right)->isconst)
            case _ =>
                false
        }

    /**
     * What is the value of an integer expression?  Only needs to be valid
     * if the expression is an integer constant (see isconst above) and is
     * defined (eg, no divide by zero.) Returns zero in all other cases.
     * FIXME: Ignores issues of overflow.
     */
    lazy val value : Expression => Int =
        attr {
            case IdnExp (n) =>
                (n->entity) match {
                    case Constant (_, p) => (p.exp)->value
                    case _               => 0 // Dummy
                }
            case IntExp (n)    => n
            case NegExp (e)    => -1 * (e->value)
            case SubExp (l, r) => (l->value) - (r->value)
            case AddExp (l, r) => (l->value) + (r->value)
            case MulExp (l, r) => (l->value) * (r->value)
            case DivExp (l, r) => if (r->value == 0)
                                      0 // Dummy
                                  else
                                      (l->value) / (r->value)
            case ModExp (l, r) => if (r->value == 0)
                                      0 // Dummy
                                  else
                                      (l->value) % (r->value)
            case _             => 0 // Dummy
        }

}
