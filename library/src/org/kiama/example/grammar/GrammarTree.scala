/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2015 Anthony M Sloane, Macquarie University.
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
 * Simple context-free grammar abstract syntax.
 */
object GrammarTree {

    import org.kiama.relation.Tree
    import scala.collection.immutable.Seq

    /**
     * Tree type for grammars.
     */
    type GrammarTree = Tree[GrammarNode,Grammar]

    /**
     * Abstract syntax tree nodes.
     */
    sealed abstract class GrammarNode extends Product

    /**
     * Grammars.
     */
    case class Grammar (startRule : Rule, rules : Seq[Rule]) extends GrammarNode

    /**
     * Production rules.
     */
    case class Rule (lhs : NonTermDef, rhs : ProdList) extends GrammarNode

    /**
     * Production lists.
     */
    sealed abstract class ProdList extends GrammarNode

    /**
     * Empty symbol list.
     */
    case class EmptyProdList () extends ProdList

    /**
     * Non-empty symbol list.
     */
    case class NonEmptyProdList (head : Prod, tail : ProdList) extends ProdList

    /**
     * Production.
     */
    case class Prod (symbols : SymbolList) extends GrammarNode

    /**
     * Symbol lists.
     */
    sealed abstract class SymbolList extends GrammarNode

    /**
     * Empty symbol list.
     */
    case class EmptySymbolList () extends SymbolList

    /**
     * Non-empty symbol list.
     */
    case class NonEmptySymbolList (head : Symbol, tail : SymbolList) extends SymbolList

    /**
     * Grammar symbols.
     */
    sealed abstract class Symbol extends GrammarNode

    /**
     * Terminal symbol.
     */
    case class TermSym (name : String) extends Symbol

    /**
     * Non-terminal symbol.
     */
    case class NonTermSym (nt : NonTermUse) extends Symbol

    /**
     * A non-terminal reference.
     */
    sealed abstract class NonTerm extends GrammarNode {
        def name : String
    }

    /**
     * Non-terminal defining occurrence.
     */
    case class NonTermDef (name : String) extends NonTerm

    /**
     * Non-terminal applied occurrence.
     */
    case class NonTermUse (name : String) extends NonTerm

    /**
     * End of input terminal assumed to appear at the end of any sentential form.
     */
    val EOI = TermSym ("$")

    // Smart constructors

    /**
     * Smart constructor for rules.
     */
    def mkRule (lhs : NonTermDef, prods : Prod*) : Rule =
        Rule (lhs, mkProdList (prods.toIndexedSeq))

    /**
     * Smart constructor for production lists.
     */
    def mkProdList (prods : Seq[Prod]) : ProdList =
        if (prods == Seq ())
            EmptyProdList ()
        else
            NonEmptyProdList (prods.head, mkProdList (prods.tail))

    /**
     * Smart constructor for productions.
     */
    def mkProd (rhs : Symbol*) : Prod =
        Prod (mkSymbolList (rhs.toIndexedSeq))

    /**
     * Smart constructor for symbol lists.
     */
    def mkSymbolList (symbols : Seq[Symbol]) : SymbolList =
        if (symbols == Seq ())
            EmptySymbolList ()
        else
            NonEmptySymbolList (symbols.head, mkSymbolList (symbols.tail))

}
