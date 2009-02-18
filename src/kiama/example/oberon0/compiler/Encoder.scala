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

            case WhileStatement (condexp, bodystmts) => EncodeWhileStmt (condexp, bodystmts)

            case ProcedureCall (desig, aps) => EncodeProcedureCall (desig, aps)
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
                    val tempReg = processNumExp (exp)

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

    // Resolve a numeric expression into a register number
    def processNumExp (exp : Exp) : Byte = {

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
                reg = processNumExp (ue.getExp)
                
                exp match {
                    case n : Neg => Assembler.emit ( MULI (reg, reg, -1))
                    case p : Pos => ()		// Do nothing
                }
                reg
            }

            case be : BinaryNumExp => {
                reg = processNumExp (be.getLeft)
                val rreg = processNumExp (be.getRight)

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
        }
    }

    // processBoolExp (saying where to go if true)
    def processBoolExp (exp : Exp, truelbl : Int, negate : Boolean) {

        exp match {

            // Not 
            case Not (e) => processBoolExp (e, truelbl, true)
            
            // And: Uses short-circuit evaluation
            case And (l, r) => {

                val truelbl2 = Assembler.newlabel
                val andendlbl = Assembler.newlabel

                // Process LHS
                processBoolExp (l, truelbl2, false)

                // If false (go to end)
                Assembler.emit ( BR (andendlbl))

                // If true, process RHS
                Assembler.mark (truelbl2)
                processBoolExp (r, truelbl, false)

                // Mark end-of-And
                Assembler.mark (andendlbl)
            }

            // Or: Uses short-circuit evaluation
            case Or (l, r) => {

                // Process LHS
                processBoolExp (l, truelbl, false)

                // If false, process RHS
                processBoolExp (r, truelbl, false)
            }

            // Other binary expressions
            case be : BinaryBoolExp => {
                val lreg = processNumExp (be.getLeft)
                val rreg = processNumExp (be.getRight)
                Assembler.emit ( CMP (lreg, rreg))

                if (negate)
	                exp match {
	                    case e : Equal => Assembler.emit (BNE (truelbl))
	                    case ne : NotEqual => Assembler.emit (BEQ (truelbl))
	                    case lt : LessThan => Assembler.emit (BGE (truelbl))
	                    case lte : LessThanOrEqual => Assembler.emit (BGT (truelbl))
	                    case gt : GreaterThan => Assembler.emit (BLE (truelbl))
	                    case gte : GreaterThanOrEqual => Assembler.emit (BLT (truelbl))
                     }
                else
                  exp match {
	                    case e : Equal => Assembler.emit (BEQ (truelbl))
	                    case ne : NotEqual => Assembler.emit (BNE (truelbl))
	                    case lt : LessThan => Assembler.emit (BLT (truelbl))
	                    case lte : LessThanOrEqual => Assembler.emit (BLE (truelbl))
	                    case gt : GreaterThan => Assembler.emit (BGT (truelbl))
	                    case gte : GreaterThanOrEqual => Assembler.emit (BGE (truelbl))
                    }

                RegMngr.freeReg (lreg)
                RegMngr.freeReg (rreg)
            }
        }
    }

    // EncodeAssignment
    def EncodeAssignment (desig : Exp, exp : Exp) {

        // Process destination
        val desResult = processDesig (desig)

        // Process source
        val srcReg = processNumExp (exp)

        // Make assignment
        Assembler.emit ( STW (srcReg, desResult.regno, desResult.offset))

        // Free registers
        if (desResult.regno != 0)
            RegMngr.freeReg (desResult.regno)

        RegMngr.freeReg (srcReg)
    }

    // EncodeIfStmt
    def EncodeIfStmt (condexp: Exp, thenstmts: List[Statement], elsestmts: List[Statement]) {

        // Process condition expression
        val truelbl = Assembler.newlabel

        processBoolExp (condexp, truelbl, false)

        // Action if false
        elsestmts.foreach (stmt => Encode (stmt))

        val exitlbl = Assembler.newlabel

        Assembler.emit ( BR (exitlbl))

        // Action if true
        Assembler.mark (truelbl)

        thenstmts.foreach (stmt => Encode (stmt))

        Assembler.mark (exitlbl)
    }

    // EncodeWhileStmt
    def EncodeWhileStmt (condexp: Exp, bodystmts: List[Statement]) {

        // Jump to test code
        val testlbl = Assembler.newlabel
        Assembler.emit ( BR (testlbl))

        // Output loop statements
        val looplbl = Assembler.newlabel
        Assembler.mark (looplbl)
        bodystmts.foreach (stmt => Encode (stmt))

        // Loop test
        Assembler.mark (testlbl)
        processBoolExp (condexp, looplbl, false)
    }

    // EncodeWrite
    def EncodeWrite (exp : Exp) {

        // Process expression
        val reg = processNumExp (exp)

        // Call WRD
        Assembler.emit ( WRD (reg))

        RegMngr.freeReg (reg)
    }

    // EncodeWriteLn
    def EncodeWriteLn (exp : Exp) {

        // Process expression
        EncodeWrite (exp)

        // Call WRL
        Assembler.emit ( WRL )
    }

    // EncodeProcedureCall
    def EncodeProcedureCall (desig: Exp, aps: List[Exp]) {
      
        desig match {
          case Ident(nm) if nm == "Write" => EncodeWrite (aps.head)
          case Ident(nm) if nm == "WriteLn" => EncodeWriteLn (aps.head)
          case _ => ()
        }
      
    }
}
