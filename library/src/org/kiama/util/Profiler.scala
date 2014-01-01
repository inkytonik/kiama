/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2014 Anthony M Sloane, Macquarie University.
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
package util

/**
 * Kiama-specific additions to `dsprofile` profilers.
 */
trait Profiler extends org.bitbucket.inkytonik.dsprofile.Profiler {

    import org.bitbucket.inkytonik.dsprofile.Events.{Dimension, Value}
    import org.kiama.attribution.Attributable
    import org.kiama.attribution.Attribute
    import org.kiama.rewriting.Strategy
    import org.kiama.util.Counter
    import scala.collection.immutable.Seq

    /**
     * Take any actions that need to be done at the start of reporting.
     */
    override def startReport (dimensionNames : Seq[Dimension]) {
        if (dimensionNames contains "dependencies")
            printTables = false
    }

    /**
     * Support Kiama-specific profiling dimensions.
     */
    override def dimValue (record : Record, dim : Dimension) : Value =
        dim match {

            /**
             * The `name` dimension is the string that identifies either the
             * strategy or attribute that is being profiled. They are checked
             * in that order.
             */
            case "name" =>
                val dimensions = record.dimensions
                if (dimensions contains "strategy") {
                    val strategy = dimensions ("strategy")
                    strategy match {
                        case s : Strategy =>
                            s.name
                        case _ =>
                            s"strategy dimension that is not a Strategy: $strategy"
                    }
                } else if (record.dimensions contains "attribute") {
                    val attribute = dimensions ("attribute")
                    attribute match {
                        case a : Attribute[_,_] =>
                            a.name
                        case _ =>
                            s"attribute dimension that is not an Attribute: $attribute"
                    }
                } else
                    "no strategy or attribute dimension, so no name"


            /**
             * `location` dimension is the location of the evaluation's subject
             * in the tree: root, inner node or leaf. Relies on the node being
             * an Attributable.
             */
            case "location" =>
                checkFor (record, dim, "", "subject") {
                    case a : Attributable =>
                        if (a.isRoot)
                            "Root"
                        else if (a.hasChildren)
                            "Inner"
                        else
                            "Leaf"
                    case _ =>
                        "unknown location"
                }

            /**
             * `subjectHash` gives the hash code of the `subject` dimension
             * and is useful if you can't tell the different between subjects
             * from their `toString` representation.
             */
            case "subjectHash" =>
                checkFor (record, dim, "", "subject") {
                    case s =>
                        s.##
                }

            /**
             * `depends-on` dimension is a summary of the direct dependencies
             * of an evaluation. Each dependence is summarised by the type of
             * the node where it was evaluated, the attribute that was evaluated
             * there and the step from the current node to that node.
             */
            case "depends-on" =>
                checkFor (record, dim, "AttrEval", "subject") {
                    case srcsubj =>
                        record.dirDescs.map (dst =>
                            checkFor (dst, dim, "AttrEval", "subject") {
                                case dstsubj =>
                                    val step = subjectsToStep (srcsubj, dstsubj)
                                    Dep (step, dimValue (dst, "type"),
                                         dimValue (dst, "name"))
                        }).toSet.mkString (", ")
                }

            /**
             * Output dot file for the dependencies involved in this attribute
             * evaluation.
             */
            case "dependencies" if isEventType (record, "AttrEval") =>
                printDependencyGraph (record, dim)
                ""

            case _ =>
                super.dimValue (record, dim)

        }

    /**
     * Print the dependency graph for the attribute evaluation represented
     * by `record`. The output is in dot form.
     */
    def printDependencyGraph (record : Record, dim : Dimension) {

        import scala.collection.mutable.{Set => MutableSet}

        // Set of subject nodes involved in this attribution evaluation.
        val subjects = MutableSet[Value] ()

        // Map from subject to set of attributes
        val attributes = scala.collection.mutable.HashMap[Value,MutableSet[Value]] ()

        // A link from an attribute of one node to an attribute of another
        case class Link (srcNum : Int, srcAttrName : Value,
                         dstNum : Int, dstAttrName : Value)

        // Collection of links representing direct dependencies
        val links = MutableSet[Link] ()

        // Map from subjects to unique node numbers
        val nodeNums = scala.collection.mutable.HashMap[Value,Int] ()

        // Counter for node numbers
        val nodeNumCounter = new Counter

        def attributeOf (record : Record) : Value =
            checkFor (record, dim, "AttrEval", "attribute") (_.toString)

        def subjectOf (record : Record) : Value =
            dimValue (record, "subject")

        def addAtrrName (subject : Value, attribute : Value) {
            if (! attributes.contains (subject))
                attributes (subject) = MutableSet[Value] ()
            attributes (subject).add (attribute)
        }

        // Set the node number of subject if it doesn't already have one
        def setNodeNum (subject : Value) {
            if (!nodeNums.contains (subject))
                nodeNums (subject) = nodeNumCounter.next ()
        }

        // Traverse dependencies collecting information
        val pending = scala.collection.mutable.Stack[Record] ()
        pending.push (record)
        while (!pending.isEmpty) {
            val curr = pending.pop ()
            val currAttr = attributeOf (curr)
            val currSubj = subjectOf (curr)
            setNodeNum (currSubj)
            subjects.add (currSubj)
            addAtrrName (currSubj, currAttr)
            for (desc <- curr.dirDescs) {
                val descAttr = attributeOf (desc)
                val descSubj = subjectOf (desc)
                setNodeNum (descSubj)
                links.add (Link (nodeNums (currSubj), currAttr,
                                 nodeNums (descSubj), descAttr))
                pending.push (desc)
            }
        }

        // Output the graph
        outputln ("digraph dependencies {")
        outputln ("    center = 1;")
        outputln ("    node [shape = record, height = .1];")
        for (subject <- subjects) {
            val tipe =
                subject match {
                    case p : Product => p.productPrefix
                    case _           => "unknown"
                }
            output ("    node%d [label = \"%1$d %s".format (nodeNums (subject), tipe))
            for (attribute <- attributes (subject))
                output (" | <%s> %1$s".format (attribute.toString))
            outputln ("\"];")
        }
        for (link <- links)
            outputln ("    \"node%d\":\"%s\" -> \"node%d\":\"%s\";".format (
                          link.srcNum, link.srcAttrName,
                          link.dstNum, link.dstAttrName))
        outputln ("}")
    }

    /**
     * Dependence record saying that the source attribute depends on
     * `attribute` of a node with type `type` that is the given step away.
     */
    case class Dep (step : Step, tipe : Value, attribute : Value) {
        override def toString = "%s(%s).%s".format (tipe, step, attribute)
    }

    /**
     * A single step in the evaluation of an attribute.
     */
    abstract class Step

    /**
     * A step to the parent of the current node.
     */
    case object Parent extends Step {
        override def toString = "^"
    }

    /**
     * A step to child `i` of the current node, counting from zero.
     */
    case class Child (i : Int) extends Step {
        override def toString = i.toString
    }

    /**
     * A step to the previous node in a sequence.
     */
    case object Prev extends Step {
        override def toString = "<"
    }

    /**
     * A step to the previous node in a sequence.
     */
    case object Next extends Step {
        override def toString = ">"
    }

    /**
     * A step nowhere. I.e., the dependent attribute is evaluated at the
     * current node.
     */
    case object Self extends Step {
        override def toString = "@"
    }

    /**
     * A step to a node that doesn't fit into any of the other categories.
     * This category will be used for nodes that were obtained as the
     * result of reference attributes or as values that sit outside the
     * main tree.
     */
    case object Other extends Step  {
        override def toString = "?"
    }

    /**
     * Summarise the single step between two nodes at which attributes
     * have been evaluated.
     */
    def subjectsToStep (src : Any, dst : Any) : Step =
        if (src == dst)
            Self
        else
            (src, dst) match {
                case (srca : Attributable, dsta : Attributable) =>
                    if (srca.parent eq dsta)
                        Parent
                    else if (srca.prev[Attributable] eq dsta)
                        Prev
                    else if (srca.next[Attributable] eq dsta)
                        Next
                    else
                        srca.children.indexWhere (_ eq dsta) match {
                            case -1 => Other
                            case c  => Child (c)
                        }
                case _ =>
                    Other
            }

}
