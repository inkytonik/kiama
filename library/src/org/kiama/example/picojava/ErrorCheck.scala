/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2014 Anthony M Sloane, Macquarie University.
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
import PicoJavaTree.PicoJavaTree

class ErrorCheck (val tree : PicoJavaTree) extends Attribution with
        NameResolution with TypeAnalyser with NullObjects with PredefinedTypes {

    import PicoJavaTree._
    import org.kiama.rewriting.Rewriter.collectall
    import org.kiama.util.Messaging.message
    import scala.collection.immutable.Seq

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
    val errors =
        attr (collectall {

            case a : AssignStmt if !isSubtypeOf (tipe (a.Value)) (tipe (a.Variable)) =>
                message (a, s"Can not assign a variable of type ${tipe (a.Variable).Name} to a value of type ${tipe (a.Value).Name}")

            case d : ClassDecl if hasCycleOnSuperclassChain (d) =>
                message (d, s"Cyclic inheritance chain for class ${d.Name}")

            case s : WhileStmt =>
                message (s, "Condition must be a boolean expression",
                        !isSubtypeOf (tipe (s.Condition)) (booleanType (s))) ++
                message (s, "Condition must be a value",
                         !isValue (s.Condition))

            case i : IdnUse if isUnknown (decl (i)) && (!isQualified (i) || !isUnknown (tipe (qualifier (i)))) =>
                message (i, s"Unknown identifier ${i.Name}")

        })

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
            case tree.parent (_ : Dot) => true
            case _                        => false
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
            case tree.parent (Dot (o, _)) =>
                o
            case _ =>
                sys.error ("Can not compute qualifier for non qualified names")
        }

}
