/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This file is derived from a JastAdd implementation of PicoJava, created
 * in the Department of Computer Science at Lund University.  See the
 * following web site for details:
 *
 * http://jastadd.cs.lth.se/examples/PicoJava/index.shtml
 */

package org.bitbucket.inkytonik.kiama
package example.picojava

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import PicoJavaTree.PicoJavaTree

class ErrorCheck(val tree : PicoJavaTree) extends Attribution with NameResolution with TypeAnalyser with NullObjects with PredefinedTypes {

    import PicoJavaTree._
    import java.util.ArrayList

    /**
     * All of the error messages for a program.
     *
     * public Collection Program.errors() {
     *    Collection c = new ArrayList();
     *    collectErrors(c);
     *    return c;
     * }
     *
     * public void ASTNode.collectErrors(Collection c) {
     *    for(int i = 0; i < getNumChild(); i++)
     *        getChild(i).collectErrors(c);
     * }
     *
     * public void AssignStmt.collectErrors(Collection c) {
     *     super.collectErrors(c);
     *     if(!getValue().type().isSubtypeOf(getVariable().type()))
     *         error(c, "Can not assign a variable of type " + getVariable().type().getName() +
     *               " to a value of type " + getValue().type().getName());
     * }
     *
     * public void ClassDecl.collectErrors(Collection c) {
     *    super.collectErrors(c);
     *    if(hasCycleOnSuperclassChain())
     *       error(c, "Cyclic inheritance chain for class " + getName());
     * }
     *
     * public void WhileStmt.collectErrors(Collection c) {
     *     super.collectErrors(c);
     *     if(!getCondition().type().isSubtypeOf(booleanType()))
     *         error(c, "Condition must be a boolean expression");
     *     if(!getCondition().isValue())
     *         error(c, "Condition must be a value");
     * }
     *
     * public void IdnUse.collectErrors(Collection c) {
     *     super.collectErrors(c);
     *     if(decl().isUnknown() && (!isQualified() || !qualifier().type().isUnknown()))
     *         error(c, "Unknown identifier " + getName());
     * }
     */

    // lazy val errors : ArrayList[String] = {
    def errors : ArrayList[String] = {
        val c = new ArrayList[String]
        collectErrors(tree.root, c)
        c
    }

    def error(c : ArrayList[String], s : String) {
        c.add(s)
    }

    def collectErrors(p : PicoJavaNode, c : ArrayList[String]) {

        // Collect error from p's children
        val children = p.productIterator
        while (children.hasNext) {
            children.next() match {
                case cp : PicoJavaNode =>
                    collectErrors(cp, c)
                case v : Vector[_] =>
                    v.collect {
                        case cp : PicoJavaNode =>
                            collectErrors(cp, c)
                    }
                case _ =>
                // Do nothing
            }
        }

        // Collect errors from p
        p match {
            case a : AssignStmt if (!isSubtypeOf(tipe(a.Variable))(tipe(a.Value))) =>
                error(c, s"Can not assign a variable of type ${tipe(a.Variable).Name} to a value of type ${tipe(a.Value).Name}")
            case d : ClassDecl if (hasCycleOnSuperclassChain(d)) =>
                error(c, s"Cyclic inheritance chain for class ${d.Name}")
            case s : WhileStmt if (!isSubtypeOf(tipe(s.Condition))(booleanType(s))) =>
                error(c, "Condition must be a boolean expression")
            case s : WhileStmt if (!isValue(s.Condition)) =>
                error(c, "Condition must be a value")
            case i : IdnUse if (isUnknown(decl(i)) && (!isQualified(i) || !isUnknown(tipe(qualifier(i))))) =>
                error(c, s"Unknown identifier ${i.Name}")
            case _ =>
            // Do nothing
        }

    }

    /**
     * Is this entity qualified?
     *
     * eq Program.getBlock().isQualified() = false;
     * eq Program.getPredefinedType(int i).isQualified() = false;
     * eq Dot.getIdnUse().isQualified() = true;
     * inh boolean IdnUse.isQualified();
     * inh boolean TypeDecl.isQualified();
     */
    val isQualified : IdnUse => Boolean =
        attr {
            case tree.parent(_ : Dot) => true
            case _                    => false
        }

    /**
     * What is the qualifier?
     *
     * eq Program.getBlock().qualifier() {
     *    throw new Error("Can not compute qualifier for non qualified names");
     * }
     * eq Program.getPredefinedType(int i).qualifier() {
     *    throw new Error("Can not compute qualifier for non qualified names");
     * }
     * eq Dot.getIdnUse().qualifier() = getObjectReference();
     * inh Access IdnUse.qualifier();
     * inh Access TypeDecl.qualifier();
     */
    val qualifier : IdnUse => Access =
        attr {
            case tree.parent(Dot(o, _)) =>
                o
            case _ =>
                sys.error("Can not compute qualifier for non qualified names")
        }

}
