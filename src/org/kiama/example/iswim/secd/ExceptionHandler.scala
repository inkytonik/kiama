/**
 * This file is part of Kiama.
 *
 * Copyright (C) 2010 Dominic R B Verity, Macquarie University.
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
package example.iswim.secd

/**
 * Add facilities to easily implement try...catch exception
 * handling to a basic SECD machine
 */

object ExceptionHandler {

	import SECDBase._

	/**
	 * Extra bytecode instructions for this extension
     * Instruction to set the exception handler register.
     */
    case class SetHandler() extends Instruction

}

/**
 * Trait implementing this SECD machine extension
 */
trait ExceptionHandler extends SECDBase {

    import ExceptionHandler._
    import SECDBase._

    /**
     * We implement try...catch exception handling by adding an extra
     * 'handler' register to the basic SECD machine. Exceptions are
     * raised by resuming the continuation held in this register.
     *
     * This contents of this register is stored in continuations but it
     * is not recorded in closures. This means that resuming a continuation
     * restores the exception handler which was in place when that continuation
     * was captured.
     *
     * The fact that closures do not contain a saved exception handler means
     * that the exception handler in play when we execute a closure
     * is the one that was in place at the point when the closure was entered
     * rather than the one from the point at which the closure was created.
     */

     /**
      * Type alias for the type of the exception handler register
      */
     type Handler = Continuation

    /**
     * Extra value types which come with this extension.
     *
     * An extended continuation value. By replacing the implementation
     * of all bytecodes which create and minipulate continuations we
     * arrange for these to completely replace the standard continuation
     * values of the basic machine.
     *
     * This is probably a little type unsafe.... but OK for the moment.
     */

     case class ExnContValue(
        	    s : Stack,
        	    e : Environment,
        	    c : Code,
        	    d : Dump,
        	    h : Handler
     ) extends Value with Continuation {
         override def hashCode () = super.hashCode
         override def equals(that : Any) = super.equals(that)
         override def toString () = "ExnContValue@" ++ hashCode.toHexString
         def getType : TypeValue = ContTypeValue
     }


    /**
     * The extra exception handler register.
     */
    lazy val handler = new State[Handler]("exception handler")

    /**
     * Change the partial function to evaluate a single instruction
     * to handle construction and use of our extended continuations.
     *
     * Note this partial function is orElse'd in front of the superclass
     * evaluator, so these new evaluators will replace those in SECDBase.
     */
	override def evalInst : Code ==> Unit = ({
        // Empty control sequence - so either return from a function
        // call or stop execution of the machine.
        case Nil => (dump : Dump) match {
            case ExnContValue(s,e,c,d,h) => (stack : Stack) match {
                case List(v) =>
                    dump := d
                    handler := h
                    stack := v :: s
                    envir := e
                    control := c
                case _ =>  raiseException(UnexpectedTermination)
            }
            case EmptyCont =>
                if (stack.length != 1 || !envir.isEmpty ||
                    !control.isEmpty) raiseException(UnexpectedTermination)
        }
        // Closure application
        case App() :: next => (stack : Stack) match {
            case ClosureValue(p,b,e) :: v :: tail =>
                dump := ExnContValue(tail,envir,next,dump,handler)
                stack := Nil
                envir := e + (p -> v)
                control := b
            case PrimValue(b) :: tail =>
                stack := tail
                control := b ++ next
            case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
        }
		// Enter and exit binding instructions
		case Enter(nms) :: next =>
		    if (stack.length < nms.length)
		        raiseException(StackUnderflow)
		    else {
				dump := ExnContValue(stack.drop(nms.length),envir,Nil,dump,handler)
				stack := Nil
				envir := envir ++ (nms.reverse zip stack.take(nms.length))
				control := next
		    }
		case Exit() :: next => (dump : Dump) match {
			case ExnContValue(s,e,Nil,d,h) => (stack : Stack) match {
				case List(v) =>
            	    dump := d
            	    handler := h
                	stack := v :: s
                	envir := e
                	control := next
				case _ => raiseException(UnexpectedExit)
			}
			case _ => raiseException(UnexpectedExit)
		}
		// Binding instruction for primitives
		case BindPrims(nms) :: next =>
		    if (nms.forall({ nm : String => primTable.contains(nm) })) {
			    dump := ExnContValue(stack,envir,Nil,dump,handler)
			    stack := Nil
			    envir := (nms :\ (envir : Environment))
			                { case (nm,e) => e + (nm->primTable(nm)) }
			    control := next
			} else raiseException(NonExistentPrimitive)
		// Continuation handling
		case AppCC() :: next => (stack : Stack) match {
			case ClosureValue(p,b,e) :: tail =>
				val cc = ExnContValue(tail,envir,next,dump,handler)
				dump := cc
				stack := Nil
				envir := e + (p -> cc)
				control := b
            case _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
		}
		case Resume() :: next => (stack : Stack) match {
			case ExnContValue(s,e,c,d,h) :: v :: tail =>
				dump := d
				handler := h
				stack := v :: s
				envir := e
				control := c
			case _ :: _ :: _ => raiseException(TypeError)
            case _ => raiseException(StackUnderflow)
		}
		case ResumeFromDump() :: next => (dump : Dump) match {
			case ExnContValue(s,e,c,d,h) => (stack : Stack) match {
			    case v :: tail =>
				    dump := d
				    handler := h
				    stack := v :: s
				    envir := e
				    control := c
		        case _ => raiseException(StackUnderflow)
	        }
            case EmptyCont => raiseException(DumpEmpty)
		}
		// Set the exception handler register to contain the continuation
		// on the top of the stack.
		case SetHandler() :: next => (stack : Stack) match {
		    case (h : ExnContValue) :: tail =>
		        handler := h
		        stack := tail
		        control := next
		    case _ :: _ => raiseException(TypeError)
		    case _ => raiseException(StackUnderflow)
		}
	} : Code ==> Unit) orElse super.evalInst

    /**
     * Raise a machine exception
     *
     * This handler throws the exception given as its argument
     * to the continuation currently stored in the exception handler
     * register.
     */
    override def raiseException (ex : ExceptionValue) {
        ex setPos execSrcPos
        (handler : Handler) match {
            case ExnContValue(s,e,c,d,h) =>
                dump := d
                handler := h
                stack := ex :: s
                envir := e
                control := c
            case _ => error("Machine Panic: invalid or non-existent exception handler.")
        }
    }
}
