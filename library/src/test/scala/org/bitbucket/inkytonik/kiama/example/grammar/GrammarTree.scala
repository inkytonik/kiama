/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2013-2017 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.kiama
package example.grammar

/**
 * Simple context-free grammar abstract syntax.
 */
object GrammarTree {

    import org.bitbucket.inkytonik.kiama.relation.Tree

    /**
     * Tree type for grammars.
     */
    type GrammarTree = Tree[GrammarNode, Grammar]

    /**
     * Abstract syntax tree nodes.
     */
    sealed abstract class GrammarNode extends Product

    /**
     * Grammars.
     */
    case class Grammar(startRule : Rule, rules : Vector[Rule]) extends GrammarNode

    /**
     * Production rules.
     */
    case class Rule(lhs : NonTermDef, rhs : ProdList) extends GrammarNode

    /**
     * Production lists.
     */
    sealed abstract class ProdList extends GrammarNode

    /**
     * Empty symbol list.
     */
    case class EmptyProdList() extends ProdList

    /**
     * Non-empty symbol list.
     */
    case class NonEmptyProdList(head : Prod, tail : ProdList) extends ProdList

    /**
     * Production.
     */
    case class Prod(symbols : SymbolList) extends GrammarNode

    /**
     * Symbol lists.
     */
    sealed abstract class SymbolList extends GrammarNode

    /**
     * Empty symbol list.
     */
    case class EmptySymbolList() extends SymbolList

    /**
     * Non-empty symbol list.
     */
    case class NonEmptySymbolList(head : Symbol, tail : SymbolList) extends SymbolList

    /**
     * Grammar symbols.
     */
    sealed abstract class Symbol extends GrammarNode

    /**
     * Terminal symbol.
     */
    case class TermSym(name : String) extends Symbol

    /**
     * Non-terminal symbol.
     */
    case class NonTermSym(nt : NonTermUse) extends Symbol

    /**
     * A non-terminal reference.
     */
    sealed abstract class NonTerm extends GrammarNode {
        def name : String
    }

    /**
     * Non-terminal defining occurrence.
     */
    case class NonTermDef(name : String) extends NonTerm

    /**
     * Non-terminal applied occurrence.
     */
    case class NonTermUse(name : String) extends NonTerm

    /**
     * End of input terminal assumed to appear at the end of any sentential form.
     */
    val EOI = TermSym("$")

    // Smart constructors

    /**
     * Smart constructor for rules.
     */
    def mkRule(lhs : NonTermDef, prods : Prod*) : Rule =
        Rule(lhs, mkProdList(prods.toVector))

    /**
     * Smart constructor for production lists.
     */
    def mkProdList(prods : Vector[Prod]) : ProdList =
        if (prods.isEmpty)
            EmptyProdList()
        else
            NonEmptyProdList(prods.head, mkProdList(prods.tail))

    /**
     * Smart constructor for productions.
     */
    def mkProd(rhs : Symbol*) : Prod =
        Prod(mkSymbolList(rhs.toVector))

    /**
     * Smart constructor for symbol lists.
     */
    def mkSymbolList(symbols : Vector[Symbol]) : SymbolList =
        if (symbols.isEmpty)
            EmptySymbolList()
        else
            NonEmptySymbolList(symbols.head, mkSymbolList(symbols.tail))

}
