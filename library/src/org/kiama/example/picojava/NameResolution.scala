/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2015 Anthony M Sloane, Macquarie University.
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

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.kiama
package example.picojava

import org.kiama.attribution.Attribution

trait NameResolution {

    self : Attribution with TypeAnalyser with NullObjects with PredefinedTypes =>

    import PicoJavaTree._

    def tree : PicoJavaTree

    /**
     * decl refers to the appropriate declaration of the Access,
     * or to unknownDecl if the declaration is missing.
     *
     * syn lazy Decl Access.decl();
     * eq IdnUse.decl() = lookup(getName());
     * eq Dot.decl() = getIdnUse().decl();
     */
    val decl : Access => Decl =
        attr {
            case Dot (_, n) => decl (n)
            case u : IdnUse => lookup (u.Name) (u)
        }

    /**
     * Lookup a name.
     *
     * inh Decl IdnUse.lookup(String name);
     * inh Decl Block.lookup(String name);
     * inh Decl TypeDecl.lookup(String name);
     *
     * eq Program.getBlock().lookup(String name) = localLookup(name); // lookup predefined types
     *
     * FIXME: haven't encoded this one, needed?
     * eq Program.getPredefinedType(int index).lookup(String name) = unknownDecl();
     *
     * eq Block.getBlockStmt(int index).lookup(String name) {
     *    // First, look in the local declarations
     *    if (!localLookup(name).isUnknown())
     *        return localLookup(name);
     *    // Then, look in surrounding context
     *    return lookup(name);
     * }
     *
     * eq ClassDecl.getBody().lookup(String name) {
     *    // First, look in superclass chain
     *    if (superClass() != null && !superClass().remoteLookup(name).isUnknown())
     *        return superClass().remoteLookup(name);
     *    // Then, look in surrounding context
     *    return lookup(name);
     * }
     *
     * eq Dot.getIdnUse().lookup(String name) =
     *    // Do a remote lookup on the object's type.
     *    getObjectReference().decl().type().remoteLookup(name);
     */
    val lookup : String => PicoJavaNode => Decl =
        paramAttr {
            name => {
                case tree.parent.pair (_ : Block, p : Program) =>
                    localLookup (name) (p)

                case tree.parent.pair (_ : Block, c : ClassDecl) =>
                    if ((superClass (c) != null) && (!isUnknown (remoteLookup (name) (superClass (c)))))
                        remoteLookup (name) (superClass (c))
                    else
                        lookup (name) (c)

                case tree.parent.pair (_ : BlockStmt, b : Block) =>
                    val d = localLookup (name) (b)
                    if (isUnknown (d)) lookup (name) (b) else d

                case tree.parent.pair (i : IdnUse, Dot (a, i2)) if i eq i2 =>
                    remoteLookup (name) (tipe (decl (a)))

                case tree.parent (p) =>
                    lookup (name) (p)
           }
       }

    /**
     * Look through the local declarations in a block.
     *
     * syn lazy Decl Block.localLookup(String name) {
     *     for (int k = 0; k < getNumBlockStmt(); k++) {
     *         Decl d = getBlockStmt(k).declarationOf(name);
     *         if (d != null) return d;
     *     }
     *     return unknownDecl();
     * }
     *
     * syn lazy Decl Program.localLookup(String name) {
     *     for (int k = 0; k < getNumPredefinedType(); k++) {
     *         Decl d = getPredefinedType(k).declarationOf(name);
     *         if (d != null) return d;
     *     }
     *     return unknownDecl();
     * }
     */
    val localLookup : String => PicoJavaNode => Decl =
        paramAttr {
            name => {
                case p : Program =>
                    finddecl (p, name, getPredefinedTypeList (p))
                case b : Block =>
                    finddecl (b, name, b.BlockStmts)
                case tree.parent (p) =>
                    localLookup (name) (p)
            }
        }

    /**
     * Search a sequence of block statements for a declaration matching a given name.
     * Return the matching declaration or the unknown declaration if not found.
     */
    def finddecl (t : PicoJavaNode, name : String, blockstmts : List[BlockStmt]) : Decl =
        blockstmts.collectFirst {
            case blockstmt if declarationOf (name) (blockstmt) != null =>
                declarationOf (name) (blockstmt)
        }.getOrElse (
            unknownDecl (t)
        )

    /**
     * Perform a remote name lookup.
     *
     *  Looks through declarations of this type that are accessible from outside the type
     * By default, there are no such declarations, so return unknownDecl.
     *
     * syn Decl TypeDecl.remoteLookup(String name) = unknownDecl();
     *
     * eq ClassDecl.remoteLookup(String name) {
     *     // First, look in local declarations
     *     if (!getBody().localLookup(name).isUnknown())
     *         return getBody().localLookup(name);
     *     // Then, look in the superclass chain
     *     if (superClass() != null && !superClass().remoteLookup(name).isUnknown())
     *         return superClass().remoteLookup(name);
     *     // Otherwise, return null object unknown
     *     return unknownDecl();
     * }
     */
    val remoteLookup : String => TypeDecl => Decl =
        paramAttr {
            name => {
                case c : ClassDecl =>
                    if (!isUnknown (localLookup (name) (c.Body)))
                        localLookup (name) (c.Body)
                    else if ((superClass (c) != null) && (!isUnknown (remoteLookup (name) (superClass (c)))))
                        remoteLookup (name) (superClass (c))
                    else
                        unknownDecl (c)
                case t =>
                    unknownDecl (t)
            }
        }

    /**
     *
     * syn Decl BlockStmt.declarationOf(String name) = null;
     * eq Decl.declarationOf(String name) {
     *     if (getName().equals(name)) return this;
     *     return null;
     * }
     */
    val declarationOf : String => BlockStmt => Decl =
         paramAttr {
             name => {
                 case d : Decl => if (name == d.Name) d else null
                 case _        => null
             }
         }

}
