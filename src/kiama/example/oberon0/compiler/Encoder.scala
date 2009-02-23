package kiama.example.oberon0.compiler

// class RegMngr:  Manage allocation and deallocation of registers
object RegMngr {

    val numreg : Int = 32
    val regs = new Array[Boolean] (numreg)

    // Init registers
    var i : Int = 1
    while (i < 28) {
        regs (i) = false
        i += 1
    }

    // Get free reg (between 1 and 28)
    // Reserve R0 (=zero), R28 (PC), R29 (FP), R30 (SP), R31 (LNK)
    def getFreeReg : Byte = {
        var i : Int = 1
        while (i < 28) {
            if (!regs (i)) {
                regs (i) = true
				return i.asInstanceOf[Byte]
			}
            i += 1
        }
		println ("No registers available")
		-1
    }

    // Free a register
    def freeReg (i : Byte) {
        regs (i) = false
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
    def EncodeStatement (stmt : Statement) {
        stmt match {
            case Assignment (desig, exp) => EncodeAssignment (desig, exp)

            case IfStatement (condexp, thenstmts, elsestmts) => EncodeIfStmt (condexp, thenstmts, elsestmts)

            case WhileStatement (condexp, bodystmts) => EncodeWhileStmt (condexp, bodystmts)

            case ProcedureCall (desig, aps) => EncodeProcedureCall (desig, aps)
        }
    }

    /**
     * For objects which require memory (VarDecls, RefVarDecls and FieldDecls), set byteOffset
     * For module and proc decls, set the total byteSize (re how much stack space required)
     * For procs, also assign a label
     */
    def setByteOffsets (obj : Attributable, byteOffset : Int) : Int = {

        obj match {

            case md @ ModuleDecl (_, decls, _, _, _) => {
                var varbyteOffset : Int = 0
                for (dec <- decls) {
                    varbyteOffset = setByteOffsets (dec, varbyteOffset)
                }
                md.byteSize = varbyteOffset
                println ("Module " + md.id.name + " requires " + md.byteSize + " bytes")
                byteOffset
            }

            case pd @ ProcDecl (_, fps, decls, _, _, _) => {
                var varbyteOffset : Int = 0
                for (fp <- fps) {
                    varbyteOffset = setByteOffsets (fp, varbyteOffset)
                }
                for (dec <- decls) {
                    varbyteOffset = setByteOffsets (dec, varbyteOffset)
                }
                pd.byteSize = varbyteOffset
                pd.label = Assembler.newlabel
                println ("Proc " + pd.id.name + " requires " + pd.byteSize + " bytes")
                byteOffset					// The proc does not affect the byteSize of the
                                            // enclosing module or proc
            }

            case RecordType (fldlst) => {
                var varbyteOffset : Int = 0
                for (fld <- fldlst) {
                    varbyteOffset = setByteOffsets (fld, varbyteOffset)
                }
                byteOffset
            }

            case TypeDecl (_, tp) => {
                setByteOffsets (tp, byteOffset)
                byteOffset
            }

            case vd @ VarDecl (_, tp) => {
                vd.byteOffset = byteOffset
                setByteOffsets (tp, -999)
                byteOffset + (vd->objType->byteSize)
            }

            case fd @ FieldDecl (_, tp) => {
                fd.byteOffset = byteOffset
                setByteOffsets (tp, -999)
                byteOffset + (fd->objType->byteSize)
            }

            case rvd @ RefVarDecl (_, tp) => {
                rvd.byteOffset = byteOffset
                setByteOffsets (tp, -999)
                byteOffset + 4				// Ref vars always store a 4 byte address,
                                            // regardless of the object pointed-to
            }

            case _ => byteOffset

        }
    }

    // Encode a module
    def EncodeModule(md : ModuleDecl) {

        md match {
            case md @ ModuleDecl (_, decls, stmts, _, _) => {

                // Set memory usage data for the whole program
                setByteOffsets (md, -999)

		        // Set the frame pointer (to 0)
		        Assembler.emit ( ADDI (29, 0, 0))
		
		        // Set the stack pointer
		        Assembler.emit ( ADDI (30, 0, md.byteSize))

		        // Encode each statement
		        stmts.foreach (stmt => EncodeStatement(stmt))
		
		        // Encode a RET(0) statement
		        Assembler.emit (RET (0))
		        
		        // Encode all procedure decls
		        val pdlst = decls.filter (dec => dec.isInstanceOf[ProcDecl])
                pdlst.foreach (dec => EncodeProc (dec.asInstanceOf[ProcDecl]))
            }
        }
    }

    def EncodeProc (pd : ProcDecl) {

        pd match {

            case pd @ ProcDecl (_, fps, decls, stmts, _, _) => {

		        // Emit the label
	            Assembler.mark (pd.label)

                // Backup the return address to the stack
                Assembler.emit ( PSH (31, 30, 4))

                // Backup the FP to the stack
                Assembler.emit ( PSH (29, 30, 4))

		        // Set the FP to the current SP
		        Assembler.emit ( ADDI (29, 30, 0))

		        // Set the SP to the current SP + the frame size
		        Assembler.emit ( ADDI (30, 30, pd.byteSize))

		        // Encode each statement
		        stmts.foreach (stmt => EncodeStatement (stmt))

                // Set SP to current FP
                Assembler.emit ( ADDI (30, 29, 0))

                // Restore previous FP
                Assembler.emit ( POP (29, 30, 4))

                // Restore return address
                Assembler.emit ( POP (31, 30, 4))

		        // Encode a RET(0) statement
		        Assembler.emit (RET (31))

		        // Encode all procedure decls
		        val pdlst = decls.filter (dec => dec.isInstanceOf[ProcDecl])
                pdlst.foreach (dec => EncodeProc (dec.asInstanceOf[ProcDecl]))
            }
        }
    }

    // Return-value of processDesig procedure
    case class desigResult(regno : Byte, offset : Int)

    // Resolve a designation into a memory address (register contents + offset)
    def processDesig (exp : Exp) : desigResult = {

        exp match {

            // Identifier
            case id : Ident => {

                id->decl match {

                    // Load address into an available register
                    case rvd : RefVarDecl => {
                        val reg = RegMngr.getFreeReg

                        Assembler.emit ( LDW (reg, 29, rvd.byteOffset))
                        desigResult (reg, 0)
                    }

                    // Initialize reg to the value in the FP
                    case _ => {
                        desigResult (29, (id->decl).byteOffset)
                    }
                }
            }

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
                    // If FP is current base, can use tempReg, but add on the address in FP
                    else if (leftResult.regno == 29) {
                        dstReg = tempReg
                        Assembler.emit ( ADD (dstReg, dstReg, 29))
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
            if (desResult.regno == 0 || desResult.regno == 29) 
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

                if (negate) {
                    processBoolExp (Or (Not (l), Not(r)), truelbl, false)
                }
                else {
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
            }

            // Or: Uses short-circuit evaluation
            case Or (l, r) => {

                if (negate) {
                    processBoolExp (And (Not (l), Not(r)), truelbl, false)
                }
                else {
	                // Process LHS
	                processBoolExp (l, truelbl, false)
	
	                // If false, process RHS
	                processBoolExp (r, truelbl, false)
                }
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
        if (desResult.regno != 0 && desResult.regno != 29)
            RegMngr.freeReg (desResult.regno)

        RegMngr.freeReg (srcReg)
    }

    // EncodeIfStmt
    def EncodeIfStmt (condexp: Exp, thenstmts: List[Statement], elsestmts: List[Statement]) {

        // Process condition expression
        val truelbl = Assembler.newlabel

        processBoolExp (condexp, truelbl, false)

        // Action if false
        elsestmts.foreach (stmt => EncodeStatement (stmt))

        val exitlbl = Assembler.newlabel

        Assembler.emit ( BR (exitlbl))

        // Action if true
        Assembler.mark (truelbl)

        thenstmts.foreach (stmt => EncodeStatement (stmt))

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
        bodystmts.foreach (stmt => EncodeStatement (stmt))

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

    // EncodeRead
    def EncodeRead (desig : Exp) {

        // Get a register to put the result of the read
        val reg = RegMngr.getFreeReg

        // Read a value
        Assembler.emit ( RD (reg))

        // Process destination
        val desResult = processDesig (desig)

        // Make assignment
        Assembler.emit ( STW (reg, desResult.regno, desResult.offset))

        // Free registers
        if (desResult.regno != 0 && desResult.regno != 29)
            RegMngr.freeReg (desResult.regno)

        RegMngr.freeReg (reg)
    }

    // Encode procedure parameters
    def ProcessActualParams (fps : List[Declaration], aps : List[Exp]) {

        if (!fps.isEmpty) {

            val fp = fps.head
            val ap = aps.head

            // Note:  Can't just push the parameters onto the stack because
            // this would break the logic in EncodeProc()
            fp match {
                case rvd : RefVarDecl => {
                    val desResult = processDesig (ap)

                    // Store parameter address
                    var reg : Byte = 0

                    // If no register allocated, allocate one
                    if (desResult.regno == 0 || desResult.regno == 29) {
                        reg = RegMngr.getFreeReg
                    }
                    // Otherwise use the same reg returned from processDesig
                    else {
                        reg = desResult.regno
                    }

                    Assembler.emit ( ADDI (reg, desResult.regno, desResult.offset))

                    // The +8 here is to skip over where the backups of LNK and
                    // the old FP goes
                    Assembler.emit ( STW (reg, 30, fp.byteOffset + 8))

                    // Free registers
                    RegMngr.freeReg (reg)
                }

                case _ => {
                    val reg = processNumExp (ap)

                    // The +8 is as above
                    Assembler.emit ( STW (reg, 30, fp.byteOffset + 8))
                    RegMngr.freeReg (reg)
                }
            }

            ProcessActualParams (fps.tail, aps.tail)
        }
    }

    // EncodeProcedureCall
    def EncodeProcedureCall (desig: Exp, aps: List[Exp]) {

        desig match {
          case Ident(nm) if nm == "Write" => EncodeWrite (aps.head)
          case Ident(nm) if nm == "WriteLn" => EncodeWriteLn (aps.head)
          case Ident(nm) if nm == "Read" => EncodeRead (aps.head)
          case id : Ident => {
              val pd = (id->decl).asInstanceOf[ProcDecl]
              pd match {
                  case ProcDecl (_, fps, _, _, _, _) => {
                      ProcessActualParams (fps, aps)
                  }
              }
              Assembler.emit ( BSR (pd.label))
          }
          case _ => ()
        }

    }
}
