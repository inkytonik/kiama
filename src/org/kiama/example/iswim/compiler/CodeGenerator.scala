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
package example.iswim.compiler

/**
 * ISWIM to SECD bytecode code generator
 */

import scala.util.parsing.input.Positional
import org.kiama.attribution.Attributable

import org.kiama.example.iswim.secd._

trait CodeGenerator {

    import org.kiama.attribution.Attribution._
    import org.kiama.util.Messaging._

    import Syntax._

    import SECDBase._
    import IntegerOps._
    import BooleanOps._
    import IntComparisonOps._
    import StackOps._
    import StringOps._
    import HeapOps._
    import RecordOps._

    /**
     * The following attribute contains the SECD bytecode generated from the ISWIM
     * syntax node it is attached to.
     */
    val code : Iswim ==> CodeTree = attr {
        case n : Iswim => positionBlock(n.pos) { n match {
    	    /**
    	     * Variable Identifiers
    	     */
    	    case Variable(s) => n.parent match {
    	        case Binding(_,_) if n isFirst => CodeTree()
    	        case _ => Lookup(s)
    	    }

            /**
             * The empty tuple
             */
            case Empty() => PushEmpty()

    	    /**
    	     * Integer Expressions
    	     */
    	    case NumVal(i) => PushInt(i)
    	    case Negate(e) => CodeTree(PushInt(0), e->code, Sub())
            case Plus(l,r) => CodeTree(l->code, r->code, Add())
        	case Minus(l,r) => CodeTree(l->code, r->code, Sub())
        	case Times(l,r) => CodeTree(l->code, r->code, Mult())
        	case Divide(l,r) => CodeTree(l->code, r->code, Div())
        	case Remainder(l,r) => CodeTree(l->code, r->code, Rem())

        	/**
        	 * Integer Comparisons
        	 */
        	case Equal(l,r) => CodeTree(l->code, r->code, Equals())
        	case NotEqual(l,r) => CodeTree(l->code, r->code, Equals(), Test(PushFalse(),PushTrue()))
        	case Less(l,r) => CodeTree(l->code, r->code, LessThan())
        	case LessEq(l,r) => CodeTree(l->code, r->code, LessThanOrEqual())
        	case Greater(l,r) => CodeTree(l->code, r->code, Swap(1,1), LessThan())
        	case GreaterEq(l,r) => CodeTree(l->code, r->code, Swap(1,1), LessThanOrEqual())

        	/**
        	 * Boolean Expressions
        	 */
        	case BoolVal(b) => if (b) PushTrue() else PushFalse()
        	// Evaluation of not implemented using Test()
        	case Not(e) => CodeTree(e->code, Test(PushFalse(),PushTrue()))
    	    // Evaluation of & and | is implemented using Test() and is shortcircuited.
        	case And(l,r) => CodeTree(l->code, Test(code(r),PushFalse()))
        	case Or(l,r) => CodeTree(l->code, Test(PushTrue(),r->code : CodeTree))

        	// Notice here that the attribute syntax r->code does not play well with our
        	// implicit conversion to CodeSegments. The problem appears to be a clash with
        	// the -> syntax for pairs (cf Map) which confuses the implicits mechanism.
        	// So we must use the applicative syntax code(r) or an explicitly types expression
        	// (r->code : CodeTree) in places where we want to convert an attribute expression
        	// implicitly to a CodeSegment.

            /**
             * String literals.
             */
            case StringVal(s) => PushString(s)

            /**
        	 * Binding Constructs (Expressions)
        	 */
        	case Let(binds,body) => CodeTree(
                CodeTree(binds.map({ case Binding(_,e) => e->code })),
                Enter(binds.map({ case Binding(Variable(nm),_) => nm })),
                body->code,
                Exit()
            )
            case LetRec(binds,body) => CodeTree(
                MkClosures(binds.map({
                    case Binding(Variable(fn),Lambda(Variable(pn),bdy)) =>
                        FunctionSpec(Some(fn),pn,code(bdy))
                })),
                Enter(binds.map({ case Binding(Variable(nm),_) => nm })),
                body->code,
                Exit()
            )

            /**
             * Code generation for top level statements
             */
         	case IswimProg(Nil) => PushEmpty()
         	case IswimProg(e :: _) => e->code

         	case s@Primitives(vs) => CodeTree(
         	    BindPrims(vs.map({ case Variable(nm) => nm })),
         	    if ((s isLast) || (s isRoot))
         	        PushEmpty()
         	    else
     	            s.next[Iswim]->code,
     	        Exit()
     	    )

         	case s@ExprStmt(e) => CodeTree(
         	    e->code,
         	    if ((s isLast) || (s isRoot))
         	        CodeTree()
         	    else CodeTree(
         	        Pop(1),
     	            s.next[Iswim]->code
     	        )
         	)

         	case s@LetStmt(binds) => CodeTree(
                CodeTree(binds.map({ case Binding(_,e) => e->code })),
                Enter(binds.map({ case Binding(Variable(nm),_) => nm })),
         	    if ((s isLast) || (s isRoot))
         	        PushEmpty()
         	    else
 	                s.next[Iswim]->code,
                Exit()
            )

            case s@LetRecStmt(binds) => CodeTree(
                MkClosures(binds.map({
                    case Binding(Variable(fn),Lambda(Variable(pn),bdy)) =>
                        FunctionSpec(Some(fn),pn,code(bdy))
                })),
                Enter(binds.map({ case Binding(Variable(nm),_) => nm })),
         	    if ((s isLast) || (s isRoot))
         	        PushEmpty()
         	    else
     	            s.next[Iswim]->code,
                Exit()
            )


        	/**
        	 * Function Definition and Application
        	 */
            case Lambda(Variable(pn),bdy) =>
                MkClosures(List(FunctionSpec(None,pn,code(bdy))))
            case Return(e) => CodeTree(e->code, ResumeFromDump())
            case Apply(f,e) => CodeTree(e->code, f->code, App())

            /**
        	 * Conditionals and Loops
        	 */
        	case If(e,thn,els) => CodeTree(e->code, Test(code(thn), code(els)))
        	// We translate "while" into a tail recursion.
        	case While(ctrl,body) => CodeTree(
        	    MkClosures(List(
        	        FunctionSpec(None,"@loop",CodeTree(
        	            ctrl->code,
        	            Test(
        	                CodeTree(body->code, Lookup("@loop"), Dup(1), TailApp()),
        	                PushEmpty()
        	            )
        	        ))
        	    )),
        	    Dup(1),
        	    App()
        	)

        	/**
        	 * Implementation note:
        	 *
        	 * To avoid clashes with variable names used in user programs, any new
        	 * variables introduced by the code generator are prepended with an '@'
        	 * symbol. These are not legal parts of
        	 *
        	 * This is also why the name @exceptionHandler was chosen as the default
        	 * name for the binding which contains the current exception handler.
        	 */

        	/**
        	 * Blocks
        	 */
        	case Block(Nil) => PushEmpty()
        	case Block(e :: es) =>
        	    ((e->code) /: es) { case (t : CodeTree, e : Expr) => CodeTree(t,Pop(1),e->code) }

            /**
         	 * References
         	 */
         	case MkRef(e) => CodeTree(Alloc(),Dup(1),e->code,Put())
         	case Val(e) => CodeTree(e->code, Get())
         	// For assignment we must do a little work to ensure that the value
         	// left on the stack is the value assigned.
         	case Assign(r,e) => CodeTree(r->code, e->code, Dup(1), Swap(1,2), Put())

            /**
             * Records / Tuples
             */
            case Tuple(fs) => CodeTree(CodeTree(fs.map(code)),MkRecord(fs.length))

            case MatchClause(Pattern(List(Variable(s))),e) =>
                CodeTree(Pop(1), Enter(List(s)), e->code, Exit())
            case m@MatchClause(Pattern(pat),e) =>
                CodeTree(
                    Dup(1),
                    PushInt(pat.length),
                    Equals(),
                    Test(
                        if (pat.length == 0)
                            CodeTree(
                                Pop(2),
                                e->code
                            )
                        else
                            CodeTree(
                                Pop(1),
                                UnpackRecord(),
                                Enter(pat.map({ case Variable(s) => s })),
                                e->code,
                                Exit()
                            ),
                        if (m.isLast)
                            CodeTree(
                                PushMachineException(MatchError),
                                RaiseException()
                            )
                        else
                            code(m.next[Iswim])
                    )
                )

            case Match(ctrl, Nil) => ctrl->code
            case Match(ctrl, cl::_) =>
                CodeTree(
                    ctrl->code,
                    Dup(1),
                    GetType(),
                    Dup(1),
                    PushType(RecordTypeValue),
                    Equals(),
                    Swap(1,1),
                    PushType(EmptyTypeValue),
                    Equals(),
                    Test(
                        CodeTree(
                            Pop(1),
                            PushInt(0)
                        ),
                        Test(
                            CodeTree(
                                Dup(1),
                                Fields()
                            ),
                            PushInt(1)
                        )
                    ),        // Stack now contains <record size> :: <value> :: Nil
                    cl->code  // Code for match clauses is attached to first clause in list
                )

         	/**
        	 * Continuations
        	 */
        	case CallCC(e) => CodeTree(e->code,AppCC())
        	case ThrowTo(e, c) => CodeTree(e->code,c->code,Resume())
        }}
    }
}