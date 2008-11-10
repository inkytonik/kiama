/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008 Anthony M Sloane, Macquarie University.
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

/**
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 * 
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package kiama.example.picojava

object NameResolution {

    import AbstractSyntax._
    import NullObjects._
    import PredefinedTypes._
    import TypeAnalysis._
    import kiama.attribution.Attribution._
        
    /**
     * decl refers to the appropriate declaration of the Access,
     * or to unknownDecl if the declaration is missing.
     * 
     * syn lazy Decl Access.decl();
     * eq IdUse.decl() = lookup(getName());
     * eq Dot.decl() = getIdUse().decl();
     */
    val decl : Access => Decl =
        attr {
            case Dot (_, n) => decl (n)
            case u : IdUse  => lookup (u) (u.Name) 
        }

    /**
     * Lookup a name.
     * 
     * inh Decl IdUse.lookup(String name);
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
     * eq Dot.getIdUse().lookup(String name) =
     *    // Do a remote lookup on the object's type.
     *    getObjectReference().decl().type().remoteLookup(name);
     */
    val lookup : Attributable => (String => Decl) =
        attr {
             case b : Block => name =>
                  b.parent match {
                      case p : Program   => locallookup (p) (name)
                      case c : ClassDecl =>
                          if ((superClass (c) != null) && (!isUnknown (remoteLookup (superClass (c)) (name))))
                              remoteLookup (superClass (c)) (name)
                          else
                              lookup (c) (name)
                  }
             case s : BlockStmt => name =>
                  s.parent match {
                      case b : Block => {
                          val d = locallookup (b) (name)
                          if (isUnknown (d)) lookup (b) (name) else d                      
                      }
                      case p => lookup (p) (name) 
                  }
             case i : IdUse => name =>
                  i.parent match {
                      case Dot (a, `i`) => remoteLookup (tipe (decl (a))) (name)
                      case p            => lookup (p) (name) 
                  }
             case t => name => lookup (t.parent) (name) 
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
    val locallookup : Attributable => (String => Decl) =
        attr {
             case p : Program => name => finddecl (p, name, predefinedTypes (p))
             case b : Block   => name => finddecl (b, name, b.BlockStmts)             
        }

    /**
     * Search a sequence of block statements for a declaration matching a given name.
     * Return the matching declaration or the unknown declaration if not found.
     */
    private def finddecl (t : Attributable, name : String, blockstmts : Seq[BlockStmt]) : Decl = {
         for (blockstmt <- blockstmts) {
             val d = declarationOf (blockstmt) (name)
             if (d != null) return d
         }
         unknownDecl (t)
    }
    
    /*
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
    val remoteLookup : Attributable => (String => Decl) =
        attr {
            case c : ClassDecl => name => {
                if (!isUnknown (locallookup (c.Body) (name)))
                    locallookup (c.Body) (name)
                else if ((superClass (c) != null) && (!isUnknown (remoteLookup (superClass (c)) (name))))
                    remoteLookup (superClass (c)) (name)
                else
                    unknownDecl (c)
            }
            case t : TypeDecl => _ => unknownDecl (t)
        }

    /**
     *  
     * syn Decl BlockStmt.declarationOf(String name) = null;
     * eq Decl.declarationOf(String name) {
     *     if (getName().equals(name)) return this;
     *     return null;
     * }
     */
    val declarationOf : BlockStmt => (String => Decl) =
         attr {
              case d : Decl => name => if (name == d.Name) d else null 
              case _        => _ => null
         }
    
}
