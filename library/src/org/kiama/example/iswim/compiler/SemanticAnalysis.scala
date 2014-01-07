/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2010-2014 Dominic R B Verity, Macquarie University.
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
package example.iswim.compiler

import org.kiama.util.Messaging

/**
 * Simple semantic analysis of untyped ISWIM programs
 */
class SemanticAnalysis {

    import org.kiama.attribution.Attribution._
    import org.kiama.rewriting.Rewriter.collectall
    import org.kiama.util.Messaging.{message, noMessages}
    import Syntax._

    /**
     * Simple name analysis - using an environment
     * All we need to do here is check that at use sites
     * all variables have been bound by an enclosing
     * let, letrec or function parameter.
     */
    val envir : Iswim => Map[Variable,Iswim] =
        attr {
            case e if e.isRoot => Map()
            case e => e.parent[Iswim] match {
                case n@Binding(v,_) if !(n.isRoot) => n.parent[Iswim] match {
                    case l@Let(_,_) => l->envir
                    case LetRec(bds,_) => (bds.last)->envirOut
                    case l@LetStmt(_) => l->envir
                    case LetRecStmt(bds) => (bds.last)->envirOut
                }
                case n@Lambda(v,_) => (n->envir) + (v->n)
                case n if e.isFirst => n->envir
                case _ => (e.prev[Iswim])->envirOut
            }
        }

    val envirOut : Iswim => Map[Variable,Iswim] =
        attr {
            case n@Binding(v,_) => (n->envir) + (v->n)
            case n@Pattern(ns) => (n->envir) ++ ns.map({ case v : Variable => v->n })
            case n@Primitives(ns) => (n->envir) ++ ns.map({ case v : Variable => v->n })
            case LetStmt(bds@(_::_)) => (bds.last)->envirOut
            case LetRecStmt(bds@(_::_)) => (bds.last)->envirOut
            case n => n->envir
        }

    /**
     * Check for match clauses which are unreachable because they are
     * preceeded by a clauses which match any value.
     */
    val unreachable : MatchClause => Boolean =
        attr (
            m => m.prev[Iswim] match {
                     case n@MatchClause(Pattern(ns),_) =>
                         n->unreachable || (ns.length == 1)
                     case _ => false
                 }
        )

    /**
     * Return the errors if any for the sub-tree rooted at a node.
     */
    val errors =
        attr (collectall {
            case v@Variable (s) =>
                v.parent[Iswim] match {
                    case Binding (w, _) if v eq w =>
                        noMessages
                    case _ : Pattern | _ : Primitives =>
                        noMessages
                    case _ =>
                        message (v, s"unbound variable '$s'", !(v->envir).contains (v))
                }
            case m@MatchClause(_,e) if m->unreachable =>
                message (m, "unreachable match clause")
        })

    /**
     * Return whether this node is semantically correct or not.
     */
    val isSemanticallyCorrect : Iswim => Boolean =
        attr {
            case n =>
                (n->errors).length == 0
        }

}
