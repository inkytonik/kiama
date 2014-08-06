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

import GrammarTree.GrammarTree

/**
 * Perform name analysis checks for the grammar language. Also, define
 * auxiliary properties nullability, first and follow for grammar symbols.
 * The latter are based on definitions found in the paper "Circular
 * Reference Attributed Grammars - their Evaluation and Applications", by
 * Magnusson and Hedin from LDTA 2003.
 */
class SemanticAnalyser (tree : GrammarTree) {

    import GrammarTree._
    import SymbolTable._
    import org.kiama.attribution.Attribution._
    import org.kiama.attribution.Decorators
    import org.kiama.rewriting.Rewriter.{collect, collectall}
    import org.kiama.util.Message
    import org.kiama.util.Messaging.message
    import org.kiama.util.{Entity, MultipleEntity, UnknownEntity}
    import scala.collection.immutable.{Seq, Set}

    val decorators = new Decorators (tree)
    import decorators._

    /**
     * The semantic error messages for a given tree.
     */
    val errors =
        attr (collectall {
            case n @ NonTermDef (name) if entity (n) == MultipleEntity () =>
                message (n, s"$name is defined more than once")
            case n @ NonTermUse (name) if entity (n) == UnknownEntity () =>
                message (n, s"$name is not declared")
        })

    /**
     * The `envin` contains the bindings that are defined "before" the given
     * node. There are none at the top of the tree and at the first rule.
     * Each other rule gets the `defenv` of the preceding rule.
     */
    val envin : GrammarNode => Environment =
        attr {
            case tree.prev (p) =>
                defenv (p)
            case tree.parent (p) =>
                envin (p)
            case _ =>
                rootenv ()
        }

    /**
     * The `defenv` is the environment that extends `envin` with the bindings
     * (if any) that are defined at this node.
     */
    val defenv : GrammarNode => Environment =
        attr {
            case r @ Rule (NonTermDef (s), _) =>
                val entity =
                    if (isDefinedInEnv (envin (r), s))
                        MultipleEntity ()
                    else
                        NonTerminal (r)
                define (envin (r), s, entity)
            case n =>
                envin (n)
        }

    /**
     * The `env` contains all of the entities that are visible at a node. In
     * this language, there is only one scope and all defining occurrences
     * are visible anywhere.
     */
    val env =
        downErr[Environment] {
            case tree.lastChild.pair (g : Grammar, c) =>
                defenv (c)
        }

    /**
     * The program entity referred to by a non-terminal occurrence.  We just
     * look in the environment.  If it's not there, then it's unknown.
     */
    val entity : NonTerm => Entity =
        attr {
            case nt =>
                lookup (env (nt), nt.name, UnknownEntity ())
        }

    // Auxiliary properties

    /**
     * The grammar that contains a node.
     */
    val grammar =
        downErr[Grammar] {
            case g : Grammar =>
                g
        }

    /**
     * The rule that contains a node.
     */
    val rule =
        downErr[Rule] {
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
                entity (n) match {
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
                ntuses (grammar (n)).filter (_.name == name)
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
    val nullable : GrammarNode => Boolean =
        circular (false) {

            // nullable of the start rule
            case Grammar (r, _) =>
                nullable (r)

            // nullable of the right-hand side of the rule
            case Rule (_, rhs) =>
                nullable (rhs)

            // nullable of the component productions
            case EmptyProdList () =>
                false
            case NonEmptyProdList (h, t) =>
                nullable (h) || nullable (t)

            // nullable of the component symbol lists
            case Prod (ss) =>
                nullable (ss)
            case EmptySymbolList () =>
                true
            case NonEmptySymbolList (h, t) =>
                nullable (h) && nullable (t)

            // terminals are not nullable
            case TermSym (_) =>
                false

            // Non-terminal definitions are nullable if the rule in which they
            // are defined is nullable. Uses are nullable if their associated
            // declaration is nullable.
            case NonTermSym (n) =>
                nullable (n)
            case tree.parent.pair (n : NonTermDef, p) =>
                nullable (p)
            case n : NonTermUse =>
                decl (n).map (nullable).getOrElse (false)

        }

    /**
     * FIRST set (i.e., which terminals can appear at the start of a sentence
     * derived from a non-terminal.
     */
    val first : GrammarNode => Set[TermSym] =
        circular (Set[TermSym] ()) {

            // FIRST of the start rule
            case Grammar (r, _) =>
                first (r)

            // FIRST of the right-hand side of the rule
            case Rule (_, rhs) =>
                first (rhs)

            // FIRST of the component productions
            case EmptyProdList () =>
                Set ()
            case NonEmptyProdList (h, t) =>
                first (h) union (first (t))
            case Prod (ss) =>
                first (ss)

            // empty symbol lists have no first symbols
            case EmptySymbolList () =>
                Set ()

            // non-empty symbol lists get the first of their head, and if the
            // head is nullable the first of their tail as well
            case NonEmptySymbolList (h, t) =>
                if (nullable (h)) (first (h)) union (first (t)) else first (h)

            // FIRST of a terminal is that terminal
            case n : TermSym =>
                Set (n)

            // FIRST of a non-terminal definition is the first of the rule in
            // which it appears. first of a use is the first of their associated
            // declaration
            case NonTermSym (n) =>
                first (n)
            case tree.parent.pair (n : NonTermDef, p) =>
                first (p)
            case n : NonTermUse =>
                decl (n).map (first).getOrElse (Set ())

        }

    /**
     * FOLLOW set (i.e., which terminals can follow this non-terminal in
     * a sentential form).
     */
    val follow : NonTerm => Set[TermSym] =
        circular (Set[TermSym] ()) {

            // FOLLOW of all uses of this defined non-terminal
            case n : NonTermDef =>
                uses (n).flatMap (follow).toSet

            // FIRST of the following symbol list, plus if the following symbol list
            // is NULLABLE, then also the FOLLOW of the LHS of the rule in which
            // this use appears
            case tree.parent.pair (n : NonTermUse, tree.next (suffix)) =>
                if (nullable (suffix))
                    first (suffix).union (follow (rule (n).lhs))
                else
                    first (suffix)

        }

}
