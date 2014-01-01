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

object ErrorCheck {

    import NameResolution._
    import PicoJavaTree._
    import PredefinedTypes._
    import TypeAnalysis._
    import org.kiama.attribution.Attribution._
    import org.kiama.util.Patterns.HasParent
    import scala.collection.immutable.Seq

    type Errors = scala.collection.mutable.Builder[String,Seq[String]]

    /**
     * All of the error messages for a program.
     *
     * public Collection Program.errors() {
     *    Collection c = new ArrayList();
     *    collectErrors(c);
     *    return c;
     * }
     */
    val errors : Program => Seq[String] =
        p => {
            val b = Seq.newBuilder[String]
            p->collectErrors (b)
            b.result ()
        }

    /**
     * Collect the errors for a node recursively.
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
    val collectErrors : Errors => PicoJavaTree => Unit =
        // NOTE: Not using paramAttr here, since we don't want caching for this
        c => (
            t => {
                // Process the errors of the children of t
                for (child <- t.children)
                    child.asInstanceOf[PicoJavaTree]->collectErrors (c)
                // Process the errors at t
                t match {
                    case a : AssignStmt =>
                        if (!isSubtypeOf (a.Value->tipe) (a.Variable->tipe))
                            a->record (c, s"Can not assign a variable of type ${(a.Variable->tipe).Name} to a value of type ${(a.Value->tipe).Name}")
                    case d : ClassDecl =>
                        if (hasCycleOnSuperclassChain (d))
                            d->record (c, s"Cyclic inheritance chain for class ${d.Name}")
                    case s : WhileStmt =>
                        if (!isSubtypeOf (s.Condition->tipe) (booleanType (s)))
                            s->record (c, "Condition must be a boolean expression")
                        if (!isValue (s.Condition))
                            s->record (c, "Condition must be a value")
                    case i : IdnUse =>
                        if (isUnknown (i->decl) &&
                            (!isQualified (i) || !isUnknown (i->qualifier->tipe)))
                        i->record (c, s"Unknown identifier ${i.Name}")
                    case _ =>
                }
            }
        )

    /**
     * Record a new error in the collection.
     *
     * protected void ASTNode.error(Collection c, String s) {
     *    //c.add(getLine(getStart()) + ": " + s);
     *    c.add(s);
     * }
     */
    val record : (Errors,String) => PicoJavaTree => Unit =
        (c,s) => a => c += s"${a.start}: $s"

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
            case HasParent (_, _ : Dot) => true
            case _                      => false
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
            case HasParent (_, Dot (o, _)) =>
                o
            case _ =>
                sys.error ("Can not compute qualifier for non qualified names")
        }

}
