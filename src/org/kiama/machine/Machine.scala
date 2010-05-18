/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2010 Anthony M Sloane, Macquarie University.
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

package org.kiama.machine

import scala.collection.mutable.HashMap
import org.kiama.util.PrettyPrinter
import org.kiama.util.PrettyPrintable

/**
 * A deterministic abstract state machine defined by its main rule and
 * called name.
 */
abstract class Machine (val name : String) {

    /**
     * Debug flag. Set this to true in sub-classes or objects to obtain
     * tracing information during execution of the machine.
     */
    def debug = false

    /**
     * A scalar item of abstract state machine state holding a value of
     * type T and called sname.
     */
    class State[T] (sname : String) extends PrettyPrintable {

        /**
         * The value of this item of state.  None means undefined.
         */
        protected var _value : Option[T] = None

        /**
         * Is this state item undefined or not?
         */
        def isUndefined : Boolean = _value isEmpty

        /**
         * Make this state item undefined.
         */
        def undefine =
            _value = None

        /**
         * Return the value of this state item if it's defined.  Otherwise
         * abort execution.
         */
        def value : T =
            _value match {
                case None     =>
                    error ("State.value: " + name + "." + sname + " is undefined")
                case Some (t) =>
                    t
            }

        /**
         * Update this item of state to the value t.  The update is actually
         * delayed until the end of the step when all updates in that
         * step happen simultaneously (along with consistency checking).  The
         * state value only becomes defined when this latter process happens.
         */
        def := (t : T) {
            updates = new ScalarUpdate (this, t) :: updates
            if (debug) {
                val p = new PrettyPrinter
                p.text (name)
                p.text (" new update: ")
                p.text (sname)
                p.text (" := ")
                p.indent {
                    this.pretty (p, t)
                }
                println (p)
	        }
        }
        
        /**
         * Pretty printer for values of type T.
         */
        def pretty (p : PrettyPrinter, t : T) : Unit =
            p.text (t.toString)
        
        /**
         * Pretty printer for the contents of this state object.
         */
        override def pretty (p : PrettyPrinter) {
            _value match {
                case Some (t) => pretty (p,t)
                case None     => p.text ("** undefined **")
            }
        }

        /**
         * Change this item of state to the value t.  The change occurs
         * immediately.
         */
        def change (t : T) =
            _value = Some (t)

        /**
         * Equality on the underlying value.  If this state item is undefined
         * then it's not equal to anything.
         */
        def =:= (t : T) : Boolean =
        	if (isUndefined)
        		false
        	else
        		_value == Some (t)

        /**
         * Make a printable representation.
         */
        override def toString : String =
            sname + " = " +
                (_value match {
                    case None     => "undef"
                    case Some (t) => t
                 })

    }

    /**
     * Implicitly allow a scalar state value of type T to be used as a value
     * of type T.
     */
    implicit def stateTToT[T] (t : State[T]) : T = t.value

    /**
     * Utility class for updaters for values of parameterised state.
     */
    class Updater[T,U] (val state : ParamState[T,U], val t : T) {

        /**
         * Update this item of state to the value u at parameter t.  The update
         * is actually delayed until the end of the step when all updates in that
         * step happen simultaneously (along with consistency checking).  The
         * state value only becomes defined when this latter process happens.
         */
        def := (u : U) = {
            updates = new ParamUpdate (state, t, u) :: updates
            if (debug)
                println (name + "." + state.sname + " (" + t + ") := " + u)
        }

        /**
         * Equality on the underlying value.  If this state item is undefined
         * then it is not equal to anything.
         */
        def =:= (u : U) : Boolean =
        	if (state.isUndefined (t))
        		false
        	else
        		state.value (t) == u

        /**
         * Make a printable representation.
         */
        override def toString : String =
        	state.value (t).toString

    }

    /**
     * A parameterised item of abstract state machine state holding values
     * of type U, associated with parameters of type T.
     */
    class ParamState[T,U] (val sname : String) extends State[HashMap[T,U]] (sname) {

        /**
         * Is this state item undefined at t or not ?
         */
        def isUndefined (t : T) : Boolean =
            _value match {
                case None     => true
                case Some (m) => ! (m contains t)
            }

        /**
         * Return an updater for the value at parameter t.  Used as s (t)
         * this will return the value in the state s at parameter t.  Used
         * as s (t) := u this will update the value to u at parameter t.
         * The update is actually delayed until the end of the step when all
         * updates in that step happen simultaneously (along with consistency
         * checking).  The state value only becomes defined when this latter
         * process happens.
         */
        def apply (t : T) = new Updater (this, t)

        /**
         * Return the value of this state item if it's defined at parameter t.
         * Otherwise abort execution.
         */
        def value (t : T) : U =
            _value match {
                case None =>
                    error ("ParamState.value: " + name + "." + sname +
                    	   " is undefined")
                case Some (m) if m contains t =>
                    m (t)
                case _ =>
                    error ("ParamState.value: " + name + "." + sname + "(" + t +
                    	   ") is undefined")
            }

        /**
         * Change this item of state to the value u at parameter t.  The
         * change occurs immediately.
         */
        def change (t : T, u : U) =
            _value match {
                case None     => _value = Some (HashMap ((t, u)))
                case Some (m) => m += ((t, u))
            }

        /**
         * Make a printable representation.
         */
        override def toString : String = {
            val p : PrettyPrinter = new PrettyPrinter
            p.text (name)
            p.text (" = ")
            p.indent {
                p.newline
                this.pretty (p)
            }
            p.toString
        }

    }

    /**
     * Allow an updater to be used to access a parameterised state value.
     */
    implicit def updaterToU[T,U] (up : Updater[T,U]) : U =
        up.state.value (up.t)

    /**
     * An update of an item of state s to have the value t.
     */
    abstract class Update {

        /**
         * Perform this update
         */
        def perform

    }

    /**
     * An update of a scalar item of state s to have the value t.
     */
    class ScalarUpdate[T] (s : State[T], t : T) extends Update {

        /**
         * Perform this update.
         */
        def perform {
            s.change (t)
        }

    }

    /**
     * An update of a parameterised item of state s to have the value u
     * at parameter t.
     */
    class ParamUpdate[T,U] (s : ParamState[T,U], t : T, u : U) extends Update {

        /**
         * Perform this update.
         */
        def perform {
            s.change (t, u)
        }

    }

    /**
     * The updates for the current step of execution of this machine.
     */
    private var updates : List[Update] = Nil

    /**
     * Initialise the state of this machine.  This routine is called
     * before the first step of the machine is attempted.  To initialise
     * state, define updates in an override of this routine and call
     * performUpdates.  Default: do nothing.
     */
    def init

    /**
     * The rule to execute to run one step of this machine.
     */
    def main

    /**
     * Clean up after this machine.  This routine is called after the
     * machine terminates.  Default: do nothing.
     */
    def finit = { }

    /**
     * Perform any pending updates, returning true if updates were
     * performed and false otherwise.
     */
    def performUpdates =
        if (updates isEmpty)
            false
        else {
            // FIXME: check updates for consistency
            updates.map (_.perform)
            true
        }

    /**
     * Perform a step of this machine.  Return true if some updates were
     * made or false if none.
     */
    def step : Boolean = {
        updates = Nil
        main
        performUpdates
    }

    /**
     * Execute the steps of this machine.  Halt when a step makes no
     * updates.  init should be called before this method.
     */
    def steps = {
        var nsteps = 0
        do {
            if (debug) {
                println (name + " step " + nsteps)
                nsteps += 1
            }
        } while (step)
    }

    /**
     * Run this machine by initialising its state and then executing
     * its steps.
     */
    def run = {
        init
        steps
        finit
    }

}
