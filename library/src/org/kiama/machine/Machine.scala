/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2013 Anthony M Sloane, Macquarie University.
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
import org.kiama.util.Emitter
import org.kiama.output.PrettyPrinter

/**
 * A deterministic abstract state machine defined by its main rule and
 * called `name`.  Tracing messages are output to the given emitter, which
 * defaults to standard output. You should avoid accessing a single
 * machine frmom multiple threads since the machine encapsulates
 * state and updates. Machines are designed to be used in a single-threaded
 * fashion.
 */
abstract class Machine (val name : String, emitter : Emitter = new Emitter)
        extends PrettyPrinter {

    import scala.annotation.tailrec
    import scala.collection.immutable.Seq
    import scala.collection.mutable.{Map => MutableMap}
    import scala.language.implicitConversions

    /**
     * Debug flag. Set this to true in sub-classes or objects to obtain
     * tracing information during execution of the machine.
     */
    def debug : Boolean = false

    /**
     * A scalar item of abstract state machine state holding a value of
     * type `T` and called `sname`.
     */
    class State[T] (val sname : String) {

        /**
         * The value of this item of state.  `None` means undefined.
         */
        protected var _value : Option[T] = None

        /**
         * Is this state item undefined or not?
         */
        def isUndefined : Boolean = _value.isEmpty

        /**
         * Make this state item undefined.
         */
        def undefine {
            _value = None
        }

        /**
         * Return the value of this state item if it's defined.  Otherwise
         * abort execution.
         */
        def value : T =
            _value match {
                case None     =>
                    sys.error (s"State.value: $name.$sname is undefined")
                case Some (t) =>
                    t
            }

        /**
         * Update this item of state to the value `t`.  The update is actually
         * delayed until the end of the step when all updates in that
         * step happen simultaneously (along with consistency checking).  The
         * state value only becomes defined when this latter process happens.
         */
        def := (t : T) {
            updates = new ScalarUpdate (this, t) +: updates
        }

        /**
         * Change this item of state to the value `t`.  The change occurs
         * immediately.
         */
        def change (t : T) {
            _value = Some (t)
        }

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
         * Make a printable representation for the contents of this state
         * object.
         */
        override def toString : String =
            _value match {
                case None     => "** undefined **"
                case Some (t) => t.toString
             }

    }

    /**
     * Implicitly allow a scalar state value of type `T` to be used as a value
     * of type `U` where `U` is a supertype of `T`.
     */
    implicit def stateTToT[T,U >: T] (t : State[T]) : U = t.value

    /**
     * Utility class for updaters for values of parameterised state.
     */
    class ParamUpdater[T,U] (val state : ParamState[T,U], val t : T) {

        /**
         * Update this item of state to the value `u` at parameter `t`.  The update
         * is actually delayed until the end of the step when all updates in that
         * step happen simultaneously (along with consistency checking).  The
         * state value only becomes defined when this latter process happens.
         */
        def := (u : U) {
            updates = new ParamUpdate (state, t, u) +: updates
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
         * Make a printable representation of the value of the parameterised
         * state item that would be updated.  Used particularly to access the
         * value of a parameterised state item.
         */
        override def toString : String =
            state.value (t).toString

    }

    /**
     * A parameterised item of abstract state machine state holding values
     * of type `U`, associated with parameters of type `T`.
     */
    class ParamState[T,U] (val psname : String) extends State[MutableMap[T,U]] (psname) {

        /**
         * Is this state item undefined at `t` or not ?
         */
        def isUndefined (t : T) : Boolean =
            _value match {
                case None     => true
                case Some (m) => ! (m contains t)
            }

        /**
         * Make this state item undefined at `t`.
         */
        def undefine (t : T) {
            _value match {
                case None     => // Nothing to undefine
                case Some (m) => _value = Some (m - t)
            }
        }

        /**
         * Return an updater for the value at parameter `t`.  Used as `s(t)`
         * this will return the value in the state `s` at parameter `t`.  Used
         * as `s(t) := u` this will update the value to `u` at parameter `t`.
         * The update is actually delayed until the end of the step when all
         * updates in that step happen simultaneously (along with consistency
         * checking).  The state value only becomes defined when this latter
         * process happens.
         */
        def apply (t : T) : ParamUpdater[T,U] =
            new ParamUpdater (this, t)

        /**
         * Return the value of this state item if it's defined at parameter `t`.
         * Otherwise abort execution.
         */
        def value (t : T) : U =
            _value match {
                case None =>
                    sys.error (s"ParamState.value: $name.$sname is undefined")
                case Some (m) if m contains t =>
                    m (t)
                case _ =>
                    sys.error (s"ParamState.value: $name.$sname($t) is undefined")
            }

        /**
         * Change this item of state to the value u at parameter `t`.  The
         * change occurs immediately.
         */
        def change (t : T, u : U) {
            _value match {
                case None     => _value = Some (MutableMap ((t, u)))
                case Some (m) => m += ((t, u))
            }
        }

    }

    /**
     * Allow an updater to be used to access a parameterised state value.
     */
    implicit def paramUpdaterToU[T,U,V >: U] (up : ParamUpdater[T,U]) : V =
        up.state.value (up.t)

    /**
     * An update of an item of state `s` to have the value `t`.
     */
    abstract class Update {

        /**
         * Perform this update
         */
        def perform : Unit

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
     * An update of a scalar item of state `s` to have the value `t`.
     */
    class ScalarUpdate[T] (s : State[T], t : T) extends Update {

        /**
         * Perform this update.
         */
        def perform {
            s.change (t)
            if (debug) {
                val d = name <> '.' <> s.sname <+> ":=" </> nest (s.toDoc)
                emitter.emitln (pretty (d))
            }
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
         * Convert to string representation.  Really only used when
         * printing inconsistent state exceptions.
         */
        override def toString : String =
            s"$name.${s.sname} := $t"

    }

    /**
     * An update of a parameterised item of state `s` to have the value `u`
     * at parameter `t`.
     */
    class ParamUpdate[T,U] (s : ParamState[T,U], t : T, u : U) extends Update {

        /**
         * Perform this update.
         */
        def perform {
            s.change (t, u)
            if (debug) {
                val d = name <> '.' <> s.sname <> '(' <> t.toDoc <> ')' <+>
                            ":=" </> nest (u.toDoc)
                emitter.emitln (pretty (d))
            }
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
         * Convert to string representation.  Really only used when
         * printing inconsistent state exceptions.
         */
        override def toString : String =
            s"$name.${s.sname}($t) := $u"

    }

    /**
     * The updates for the current step of execution of this machine.
     */
    private var updates : Seq[Update] = Nil

    /**
     * Initialise the state of this machine.  This routine is called
     * before the first step of the machine is attempted.  Any pending
     * state updates will be performed after this routine returns.
     * Default: do nothing.
     */
    def init { }

    /**
     * The rule to execute to run one step of this machine.
     */
    def main : Unit

    /**
     * Clean up after this machine.  This routine is called after the
     * machine terminates.  Default: do nothing.
     */
    def finit { }

    /**
     * Perform any pending updates, returning true if updates were
     * performed and false otherwise.  The updates are first checked
     * for consistency.  If the same piece of state is updated more
     * than once, it must be updated to the same value by all updates.
     * If updates are not consistent, the machine is aborted.
     */
    def performUpdates : Boolean = {
        if (updates.isEmpty)
            false
        else {
            // Check updates for consistency
            updates.groupBy (_.key).map {
                case (key, keyUpdates) =>
                    if (keyUpdates.map (_.value).distinct.length != 1)
                        throw new InconsistentUpdateException (this, keyUpdates)
            }
            // Actually perform the updates
            updates.map (_.perform)
            true
        }
    }

    /**
     * Reset the machine to begin a step.
     */
    def reset {
        updates = Nil
    }

    /**
     * Perform a step of this machine.  Return true if some updates were
     * made or false if none.
     */
    def step : Boolean = {
        reset
        main
        performUpdates
    }

    /**
     * Execute the steps of this machine.  Halt when a step makes no
     * updates.  `init` should be called before this method.
     */
    @tailrec
    private def steps (nstep : Int) {
        if (debug)
            emitter.emitln (s"$name step $nstep")
        if (step)
            steps (nstep + 1)
    }

    /**
     * Run this machine by initialising its state and then executing
     * its steps.
     */
    def run {
        init
        performUpdates
        steps (0)
        finit
    }

}

/**
 * A machine has performed an inconsistent update in the sense that a step
 * has updated an item of state to two different values.  `m` is the machine
 * that performed the update, `updates` is all of the updates for the key
 * that was updated inconsistently.
 */
class InconsistentUpdateException (m : Machine, updates : Seq[Machine#Update])
    extends Exception (s"Machine = ${m.name}, updates = $updates")
