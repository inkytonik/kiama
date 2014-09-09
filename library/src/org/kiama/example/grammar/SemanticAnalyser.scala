/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2014 Anthony M Sloane, Macquarie University.
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
package example.grammar

/**
 * Perform name analysis checks for the grammar language. Also, define
 * auxiliary properties nullability, first and follow for grammar symbols.
 * The latter are based on definitions found in the paper "Circular
 * Reference Attributed Grammars - their Evaluation and Applications", by
 * Magnusson and Hedin from LDTA 2003.
 */
class SemanticAnalyser {

    import GrammarTree._
    import SymbolTable._
    import org.kiama.attribution.Attribution._
    import org.kiama.attribution.Decorators.downErr
    import org.kiama.rewriting.Rewriter.collect
    import org.kiama.util.Messaging.{collectmessages, Messages, message}
    import org.kiama.util.{Entity, MultipleEntity, UnknownEntity}
    import scala.collection.immutable.{Seq, Set}

    /**
     * The semantic error messages for a given tree.
     */
    val errors : GrammarTree => Messages =
        attr { collectmessages {
            case n @ NonTermDef (name) if n->entity == MultipleEntity () =>
                message (n, s"$name is defined more than once")
            case n @ NonTermUse (name) if n->entity == UnknownEntity () =>
                message (n, s"$name is not declared")
        }}

    /**
     * The `envin` contains the bindings that are defined "before" the given
     * node. There are none at the top of the tree and at the first rule.
     * Each other rule gets the `defenv` of the preceding rule.
     */
    val envin : GrammarTree => Environment =
        attr {
            case p : Grammar    => rootenv ()
            case n if n.isFirst => (n.parent[GrammarTree])->envin
            case n              => (n.prev[GrammarTree])->defenv
        }

    /**
     * The `defenv` is the environment that extends `envin` with the bindings
     * (if any) that are defined at this node.
     */
    val defenv : GrammarTree => Environment =
        attr {
            case r @ Rule (NonTermDef (s), _) =>
                val entity =
                    if (isDefinedInEnv (r->envin, s))
                        MultipleEntity ()
                    else
                        NonTerminal (r)
                define (r->envin, s, entity)
            case n =>
                n->envin
        }

    /**
     * The `env` contains all of the entities that are visible at a node. In
     * this language, there is only one scope and all defining occurrences
     * are visible anywhere.
     */
    val env =
        downErr[GrammarTree,Environment] {
            case g : Grammar =>
                (g.lastChild[Rule])->defenv
        }

    /**
     * The program entity referred to by a non-terminal occurrence.  We just
     * look in the environment.  If it's not there, then it's unknown.
     */
    val entity : NonTerm => Entity =
        attr {
            case nt =>
                lookup (nt->env, nt.name, UnknownEntity ())
        }

    // Auxiliary properties

    /**
     * The grammar that contains a node.
     */
    val grammar =
        downErr[GrammarTree,Grammar] {
            case g : Grammar =>
                g
        }

    /**
     * The rule that contains a node.
     */
    val rule =
        downErr[GrammarTree,Rule] {
            case r : Rule =>
                r
        }

    /**
     * Defining ocurrence that is referrred to by this applied occurrence,
     * if there is one.
     */
    val decl : NonTermUse => Option[Rule] =
        attr {
            case n =>
                n->entity match {
                    case NonTerminal (r) => Some (r)
                    case _               => None
                }
        }

    /**
     * Non-terminal uses that are applied occurrences of a given defining
     * occurrence.
     */
    val uses : NonTermDef => Seq[NonTermUse] =
        attr {
            case n @ NonTermDef (name) =>
                (n->grammar->ntuses).filter (_.name == name)
        }

    /**
     * Non-terminal applied occurrences in a tree.
     */
    val ntuses =
        attr (collect[List,NonTermUse] {
            case n : NonTermUse =>
                n
        })

    // Grammar properties

    /**
     * Nullability (i.e., can derive the empty sequence).
     */
    val nullable : GrammarTree => Boolean =
        circular (false) {

            // nullable of the start rule
            case Grammar (r, _) =>
                r->nullable

            // nullable of the right-hand side of the rule
            case Rule (_, rhs) =>
                rhs->nullable

            // nullable of the component productions
            case EmptyProdList () =>
                false
            case NonEmptyProdList (h, t) =>
                h->nullable || t->nullable

            // nullable of the component symbol lists
            case Prod (ss) =>
                ss->nullable
            case EmptySymbolList () =>
                true
            case NonEmptySymbolList (h, t) =>
                h->nullable && t->nullable

            // terminals are not nullable
            case TermSym (_) =>
                false

            // Non-terminal definitions are nullable if the rule in which they
            // are defined is nullable. Uses are nullable if their associated
            // declaration is nullable.
            case NonTermSym (n) =>
                n->nullable
            case n : NonTermDef =>
                (n.parent[Rule])->nullable
            case n : NonTermUse =>
                (n->decl).map (nullable).getOrElse (false)

        }

    /**
     * FIRST set (i.e., which terminals can appear at the start of a sentence
     * derived from a non-terminal.
     */
    val first : GrammarTree => Set[TermSym] =
        circular (Set[TermSym] ()) {

            // FIRST of the start rule
            case Grammar (r, _) =>
                r->first

            // FIRST of the right-hand side of the rule
            case Rule (_, rhs) =>
                rhs->first

            // FIRST of the component productions
            case EmptyProdList () =>
                Set ()
            case NonEmptyProdList (h, t) =>
                (h->first) union (t->first)
            case Prod (ss) =>
                ss->first

            // empty symbol lists have no first symbols
            case EmptySymbolList () =>
                Set ()

            // non-empty symbol lists get the first of their head, and if the
            // head is nullable the first of their tail as well
            case NonEmptySymbolList (h, t) =>
                if (h->nullable) (h->first) union (t->first) else h->first

            // FIRST of a terminal is that terminal
            case n : TermSym =>
                Set (n)

            // FIRST of a non-terminal definition is the first of the rule in
            // which it appears. first of a use is the first of their associated
            // declaration
            case NonTermSym (n) =>
                n->first
            case n : NonTermDef =>
                (n.parent[Rule])->first
            case n : NonTermUse =>
                (n->decl).map (first).getOrElse (Set ())

        }

    /**
     * FOLLOW set (i.e., which terminals can follow this non-terminal in
     * a sentential form).
     */
    val follow : NonTerm => Set[TermSym] =
        circular (Set[TermSym] ()) {

            // FOLLOW of all uses of this defined non-terminal
            case n : NonTermDef =>
                (n->uses).flatMap (follow).toSet

            // FIRST of the following symbol list, plus if the following symbol list
            // is NULLABLE, then also the FOLLOW of the LHS of the rule in which
            // this use appears
            case n : NonTermUse =>
                val suffix = n.parent.next[SymbolList]
                if (suffix->nullable)
                    (suffix->first).union ((n->rule).lhs->follow)
                else
                    suffix->first

        }

}
