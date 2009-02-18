package kiama.example.oberon0.compiler

// class RegMngr:  Manage allocation and deallocation of registers
object RegMngr {

    val numreg : Int = 32
    val regs = new Array[Boolean] (numreg)

    // Init registers
    var i : Int = 1
    while (i < numreg) {
        regs (i) = false
        i += 1
    }

    // Get free reg (starting at reg 1)
    def getFreeReg : Byte = {
        var i : Int = 1
        while (i < numreg) {
            if (!regs (i)) {
                regs (i) = true
				return i.asInstanceOf[Byte]
			}
            i += 1
        }
		println("No registers available")
		-1
    }

    // Free a register
    def freeReg (i : Byte) {
        regs(i) = false
    }
}

object Encoder {

    import AST._
    import NameAnalysis._
    import TypeAnalysis._
    import ConstantAnalysis._
    import ValueAnalysis._
    import kiama.attribution.Attribution._
    import kiama.example.oberon0.assembler._
    import kiama.example.oberon0.machine.RISCISA._

    // Encode an object
    def Encode (obj : Attributable) {
        obj match {
            case ModuleDecl (_, decls, stmts, _, _) => EncodeModule (decls, stmts)

            case Assignment (desig, exp) => EncodeAssignment (desig, exp)
            
            case IfStatement (condexp, thenstmts, elsestmts) => EncodeIfStmt (condexp, thenstmts, elsestmts)
        }
    }

    // Encode a module
    def EncodeModule(decls : List[Declaration], stmts : List[Statement]) {

        // Encode each statement
        stmts.foreach (stmt => Encode(stmt))

        // Encode a RET(0) statement
        Assembler.emit (RET (0))
    }

    // Return-value of processDesig procedure
    case class desigResult(regno : Byte, offset : Int)

    // Resolve a designation into a memory address (register contents + offset)
    def processDesig (exp : Exp) : desigResult = {

        exp match {

            // Identifier
            case id : Ident => desigResult (0, (id->decl).byteOffset)

            // Field designation
            case FieldDesig (left, id) => {
                val leftResult = processDesig (left)
                desigResult (leftResult.regno, leftResult.offset + (id->decl).byteOffset)
            }

            // Array designation
            case ad @ ArrayDesig (left, exp) => {
                val leftResult = processDesig (left)

                // Constant index
                if (exp->objType == IntegerType && exp->isConstant) {
                    desigResult (leftResult.regno,
                               leftResult.offset + (exp->intValue) * (ad->objType->byteSize))
                }
                // Variable index
                else {

                    // Process exp
                    val tempReg = processExp (exp)

                    // Multiply by array item size
                    Assembler.emit ( MULI (tempReg, tempReg, (ad->objType->byteSize)))

                    var dstReg : Byte = 0

                    // If no reg currently allocated, can use tempReg
                    if (leftResult.regno == 0) {
                        dstReg = tempReg
                    }
                    // Otherwise use the same reg as the LHS (but need to add index result to it)
                    else {
                        dstReg = leftResult.regno

                        // Add the result of the index calculation to this register
                        Assembler.emit ( ADD (dstReg, dstReg, tempReg))
                        RegMngr.freeReg (tempReg)
                    }

                    desigResult (dstReg, leftResult.offset)
                }
            }
        }
    }

    // Resolve an expression into a register number
    def processExp (exp : Exp) : Byte = {

        var reg : Byte = 0

        // If the expression is constant, put value in a register
        if (exp->isConstant) {

            reg = RegMngr.getFreeReg

            if (exp->objType == IntegerType)
                Assembler.emit ( ADDI (reg, 0, exp->intValue))
            else if (exp->objType == BooleanType)
                Assembler.emit ( ADDI (reg, 0, (exp->boolValue).asInstanceOf[Int]))
            
            return reg
        }

        // If the expression is non-constant, build it at runtime
        // If a designator ...
        if (exp.isInstanceOf[Ident] || exp.isInstanceOf[FieldDesig] || exp.isInstanceOf[ArrayDesig]) {

            // Get memory address as register and offset
            val desResult = processDesig (exp)

            // If no register allocated, allocate one
            if (desResult.regno == 0)
                reg = RegMngr.getFreeReg
            else
                reg = desResult.regno

            // Load the item from memory
            Assembler.emit ( LDW (reg, desResult.regno, desResult.offset))

            return reg
        }

        // Other expressions
        exp match {

            case ue : UnaryNumExp => {
                reg = processExp (ue.getExp)
                
                exp match {
                    case n : Neg => Assembler.emit ( MULI (reg, reg, -1))
                    case p : Pos => ()		// Do nothing
                }
                reg
            }

            case be : BinaryNumExp => {
                reg = processExp (be.getLeft)
                val rreg = processExp (be.getRight)

                exp match {
                    case m : Mult => Assembler.emit ( MUL (reg, reg, rreg))
                    case d : Div => Assembler.emit ( DIV (reg, reg, rreg))
                    case m : Mod => Assembler.emit ( MOD (reg, reg, rreg))
                    case p : Plus => Assembler.emit ( ADD (reg, reg, rreg))
                    case m : Minus => Assembler.emit ( SUB (reg, reg, rreg))
                }

                RegMngr.freeReg (rreg)
                reg
            }

            case n @ Not (e) => {
              
                0
            }            
            
            // And: Uses short-circuit evaluation
            case a @ And (l, r) => {

                // Test LHS
                reg = processExp (l)
                Assembler.emit ( CMPI (reg, true.asInstanceOf[Int]))
                val truelbl1 = Assembler.newlabel
                Assembler.emit (BEQ (truelbl1))

                // Action if LHS is false
                Assembler.emit ( ADDI (reg, 0, false.asInstanceOf[Int]))

                val exitlbl = Assembler.newlabel
                Assembler.emit ( BR (exitlbl))

                // Test RHS
                Assembler.mark (truelbl1)

                val rreg = processExp (r)
                Assembler.emit ( CMPI (rreg, true.asInstanceOf[Int]))
                val truelbl2 = Assembler.newlabel
                Assembler.emit (BEQ (truelbl2))

                // Action if RHS is false
                Assembler.emit ( ADDI (reg, 0, false.asInstanceOf[Int]))
                Assembler.emit ( BR (exitlbl))

                // Action if LHS and RHS are true
                Assembler.emit ( ADDI (reg, 0, true.asInstanceOf[Int]))
                Assembler.mark (exitlbl)

                RegMngr.freeReg (rreg)
                reg
            }

            case o @ Or (l, r) => {
              
                0
            }

            case be : BinaryBoolExp => {
                reg = processExp (be.getLeft)
                val rreg = processExp (be.getRight)
                Assembler.emit ( CMP (reg, rreg))
                val truelbl = Assembler.newlabel

                exp match {
                    case e : Equal => Assembler.emit (BEQ (truelbl))
                    case ne : NotEqual => Assembler.emit (BNE (truelbl))
                    case lt : LessThan => Assembler.emit (BLT (truelbl))
                    case lte : LessThanOrEqual => Assembler.emit (BLE (truelbl))
                    case gt : GreaterThan => Assembler.emit (BGT (truelbl))
                    case gte : GreaterThanOrEqual => Assembler.emit (BGE (truelbl))
                }

                // Action if false
                Assembler.emit ( ADDI (reg, 0, false.asInstanceOf[Int]))

                val exitlbl = Assembler.newlabel

                Assembler.emit ( BR (exitlbl))

                // Action if true
                Assembler.mark (truelbl)

                Assembler.emit ( ADDI (reg, 0, true.asInstanceOf[Int]))

                Assembler.mark (exitlbl)

                RegMngr.freeReg (rreg)
                reg
            }
            
        }

    }

    def EncodeAssignment (desig : Exp, exp : Exp) {

        // Process destination
        val desResult = processDesig (desig)

        // Process source
        val srcReg = processExp (exp)

        // Make assignment
        Assembler.emit ( STW (srcReg, desResult.regno, desResult.offset))

        // Free registers
        if (desResult.regno != 0)
            RegMngr.freeReg (desResult.regno)

        RegMngr.freeReg (srcReg)
    }

    def EncodeIfStmt (condexp: Exp, thenstmts: List[Statement], elsestmts: List[Statement]) {
        
        // Process condition expression
        val condReg = processExp (condexp)

        Assembler.emit ( CMPI (condReg, true.asInstanceOf[Int]))
        val truelbl = Assembler.newlabel
        Assembler.emit (BEQ (truelbl))

        // Action if false
        elsestmts.foreach (stmt => Encode (stmt))

        val exitlbl = Assembler.newlabel

        Assembler.emit ( BR (exitlbl))

        // Action if true
        Assembler.mark (truelbl)

        thenstmts.foreach (stmt => Encode (stmt))

        Assembler.mark (exitlbl)

        RegMngr.freeReg (condReg)
    }
}
