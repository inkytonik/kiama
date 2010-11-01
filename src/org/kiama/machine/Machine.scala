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

package org.kiama
package machine

import java.lang.Exception
import scala.collection.mutable.HashMap
import org.kiama.util.Emitter
import org.kiama.util.PrettyPrinter
import org.kiama.util.PrettyPrintable

/**
 * A deterministic abstract state machine defined by its main rule and
 * called name.  Tracing messages are output to the given emitter, which
 * defaults to standard output.
 */
abstract class Machine (val name : String, emitter : Emitter = new Emitter) {

    /**
     * Debug flag. Set this to true in sub-classes or objects to obtain
     * tracing information during execution of the machine.
     */
    def debug () = false

    /**
     * A scalar item of abstract state machine state holding a value of
     * type T and called sname.
     */
    class State[T] (val sname : String) extends PrettyPrintable {

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
        def undefine () =
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
                p.text (".")
                p.text (sname)
                p.text (" := ")
                p.indent {
                    this.pretty (p, t)
                }
                emitter.emitln (p)
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
                case None     => p.text ("** undefined **")
                case Some (t) => pretty (p, t)
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
            _value match {
                 case None     => "** undefined **"
                 case Some (t) => t.toString
            }

    }

    /**
     * Implicitly allow a scalar state value of type T to be used as a value
     * of type U where U is a supertype of T.
     */
    implicit def stateTToT[T,U >: T] (t : State[T]) : U = t.value

    /**
     * Utility class for updaters for values of parameterised state.
     */
    class ParamUpdater[T,U] (val state : ParamState[T,U], val t : T) extends PrettyPrintable {

        /**
         * Update this item of state to the value u at parameter t.  The update
         * is actually delayed until the end of the step when all updates in that
         * step happen simultaneously (along with consistency checking).  The
         * state value only becomes defined when this latter process happens.
         */
        def := (u : U) = {
            updates = new ParamUpdate (state, t, u) :: updates
            if (debug) {
                val p = new PrettyPrinter
                p.text (name)
                p.text (".")
                p.text (state.sname)
                p.text ("(")
                p.text (t.toString)
                p.text (")")
                p.text (" := ")
                p.indent {
                    this.pretty (p, u)
                }
                emitter.emitln (p)
	        }
        }

        /**
         * Pretty printer for values of type U.
         */
        def pretty (p : PrettyPrinter, u : U) : Unit =
            p.text (u.toString)

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
    class ParamState[T,U] (val psname : String) extends State[HashMap[T,U]] (psname) {

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
        def apply (t : T) = new ParamUpdater (this, t)

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
            p.text (".")
            p.text (psname)
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
    implicit def paramUpdaterToU[T,U,V >: U] (up : ParamUpdater[T,U]) : V =
        up.state.value (up.t)

    /**
     * An update of an item of state s to have the value t.
     */
    abstract class Update {

        /**
         * Perform this update
         */
        def perform

        /**
         * Return a key for use when checking consistency of this update
         * with other updates.  Must uniquely determine the state that is
         * being updated.
         */
        def key : AnyRef

        /**
         * Return the value to which the state is being updated.
         */
        def value : Any

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

        /**
         * Return a key for use when checking consistency of this update
         * with other updates.  Must uniquely determine the state that is
         * being updated.
         */
        def key : AnyRef = s

        /**
         * Return the value to which the state is being updated.
         */
        def value : Any = t

        /**
         * Make a printable representation.
         */
        override def toString : String =
            s.sname + " := " + t

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

        /**
         * Return a key for use when checking consistency of this update
         * with other updates.  Must uniquely determine the state that is
         * being updated.
         */
        def key : AnyRef = (s, t)

        /**
         * Return the value to which the state is being updated.
         */
        def value : Any = u

        /**
         * Make a printable representation.
         */
        override def toString : String =
            s.sname + "(" + t + ") := " + u

    }

    /**
     * The updates for the current step of execution of this machine.
     */
    private var updates : List[Update] = Nil

    /**
     * Initialise the state of this machine.  This routine is called
     * before the first step of the machine is attempted.  Any pending
     * state updates will be performed after this routine returns.
     * Default: do nothing.
     */
    def init () = { }

    /**
     * The rule to execute to run one step of this machine.
     */
    def main ()

    /**
     * Clean up after this machine.  This routine is called after the
     * machine terminates.  Default: do nothing.
     */
    def finit () = { }

    /**
     * Perform any pending updates, returning true if updates were
     * performed and false otherwise.  The updates are first checked
     * for consistency.  If the same piece of state is updated more
     * than once, it must be updated to the same value by all updates.
     * If updates are not consistent, the machine is aborted.
     */
    def performUpdates () =
        if (updates isEmpty)
            false
        else {
            // Check updates for consistency
            val m = new HashMap[AnyRef,Any]
            for (u <- updates) {
                val k = u.key
                if (m contains k) {
                    if (m (k) != u.value)
                        throw new InconsistentUpdateException (this, u, m (k))
                } else {
                    m (k) = u.value
                }
            }
            // Actually perform the updaes
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
    def steps {
        var nsteps = 0
        do {
            if (debug) {
                emitter.emitln (name + " step " + nsteps)
                nsteps += 1
            }
        } while (step)
    }

    /**
     * Run this machine by initialising its state and then executing
     * its steps.
     */
    def run {
        init
        performUpdates
        steps
        finit
    }

}

/**
 * A machine has performed an inconsistent update in the sense that a step
 * has updated an item of state to two different values.  m is the machine
 * that performed the update, u is the update and v is the value used by
 * another update of the same state item in that step.
 */
class InconsistentUpdateException[T] (m : Machine, u : Machine#Update, v : T)
    extends Exception ("Machine = " + m.name + ", update = " + u + ", other value = " + v)
