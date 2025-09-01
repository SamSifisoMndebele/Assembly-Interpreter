package asm

import cpu.CPU
import cpu.Memory
import model.Instruction
import model.Op
import model.Operand
import model.Reg

class Interpreter(private val program: List<Instruction>, private val labels: Map<String, Long>, private val mem: Memory) {
    private val cpu = CPU()

    init {
        // Initialize Stack Pointer to the top of memory. SS is 0, so SP is effectively the physical address.
        // mem.size is Int, cpu.regs values are Long.
        cpu.writeReg(Reg.SP, mem.size) // SP will be an offset; if SS is 0, it's also the physical top.
    }

    private fun addrOf(memOp: Operand.MemOp): Long {
        val offsetPart = (memOp.base?.let { cpu.readReg(it) } ?: 0) + (memOp.disp ?: 0)

        // Determine segment register: BP-based addressing uses SS, otherwise DS.
        val segmentReg = if (memOp.base == Reg.BP) Reg.SS else Reg.DS
        val segmentBase = cpu.readReg(segmentReg)

        // Physical address: (SegmentBase + Offset) & 0xFFFF (assuming 16-bit address space for now)
        // If segment registers are paragraph addresses (base * 16), this calculation would change.
        // For now, assume segment registers hold the linear base address.
        return (segmentBase + offsetPart) and 0xFFFF
    }

    private fun readOp(op: Operand, line: Int): Long = when(op) {
        // TODO: Handle different operand sizes (8, 16, 32-bit) based on op.reg or instruction type
        is Operand.ImmOp -> op.value and 0xFFFF // Assuming 16-bit operation for now
        is Operand.RegOp -> cpu.readReg(op.reg) // Reads 16-bit value from reg
        is Operand.MemOp -> mem.read16(addrOf(op)) // Reads 16-bit value from memory
        is Operand.LabelOp -> labels[op.name]?.let { it.toLong() and 0xFFFF } ?: error("Line $line: Unknown label: ${op.name}")
        else -> error("Line $line: Unimplemented operand type ${op.javaClass.simpleName}")
    }

    private fun writeOp(op: Operand, value: Long, line: Int) {
        // TODO: Handle different operand sizes (8, 16, 32-bit)
        when(op) {
            is Operand.RegOp -> cpu.writeReg(op.reg, value) // Writes 16-bit value
            is Operand.MemOp -> mem.write16(addrOf(op), value) // Writes 16-bit value
            is Operand.ImmOp -> error("Line $line: cannot write to immediate")
            is Operand.LabelOp -> error("Line $line: cannot write to a label")
        }
    }

    // TODO: Update flag calculations for 8/32-bit operations
    private fun setFlagsFromAdd(a: Long, b: Long, res: Long) { // a, b, res are treated as 16-bit
        cpu.flags.ZF = (res and 0xFFFF) == 0L
        cpu.flags.SF = (res and 0x8000) != 0L
        // Carry for unsigned 16-bit addition
        cpu.flags.CF = (a.toLong() and 0xFFFFL) + (b.toLong() and 0xFFFFL) > 0xFFFFL
        // Overflow for signed 16-bit addition (simplified, can be more robust)
        // OF = (A_sign == B_sign) && (A_sign != Res_sign)
        val aSign = (a and 0x8000) != 0L
        val bSign = (b and 0x8000) != 0L
        val resSign = (res and 0x8000) != 0L
        cpu.flags.OF = (aSign == bSign) && (aSign != resSign)
    }

    private fun setFlagsFromSub(a: Long, b: Long, res: Long) { // a, b, res are treated as 16-bit
        cpu.flags.ZF = (res and 0xFFFF) == 0L
        cpu.flags.SF = (res and 0x8000) != 0L
        // Borrow for unsigned 16-bit subtraction
        cpu.flags.CF = (a.toLong() and 0xFFFFL) < (b.toLong() and 0xFFFFL)
        // Overflow for signed 16-bit subtraction (simplified)
        // OF = (A_sign != B_sign) && (B_sign == Res_sign)
        val aSign = (a and 0x8000) != 0L
        val bSign = (b and 0x8000) != 0L
        val resSign = (res and 0x8000) != 0L
        cpu.flags.OF = (aSign != bSign) && (bSign == resSign)
    }


    private fun jccTaken(op: Op): Boolean = when(op) {
        Op.JE -> cpu.flags.ZF // Jump if Equal (ZF=1)
        Op.JNE -> !cpu.flags.ZF // Jump if Not Equal (ZF=0)
        // JG: Jump if Greater (signed) (SF=OF and ZF=0) - assuming SF holds sign, OF for overflow
        Op.JG -> (cpu.flags.SF == cpu.flags.OF) && !cpu.flags.ZF
        // JL: Jump if Less (signed) (SF!=OF)
        Op.JL -> cpu.flags.SF != cpu.flags.OF
        // JGE: Jump if Greater or Equal (signed) (SF=OF)
        Op.JGE -> cpu.flags.SF == cpu.flags.OF
        // JLE: Jump if Less or Equal (signed) (SF!=OF or ZF=1)
        Op.JLE -> (cpu.flags.SF != cpu.flags.OF) || cpu.flags.ZF
        // TODO: Add Jumps based on unsigned flags (JA, JB, JAE, JBE) using CF
        else -> false
    }

    private fun push(v: Long) { // Pushes a 16-bit value
        val currentSpOffset = cpu.readReg(Reg.SP)
        val newSpOffset = (currentSpOffset - 2) and 0xFFFF // SP is 16-bit offset
        cpu.writeReg(Reg.SP, newSpOffset)

        val ssBase = cpu.readReg(Reg.SS) // SS holds segment base
        val physicalStackAddress = (ssBase + newSpOffset) and 0xFFFF
        mem.write16(physicalStackAddress, v)
    }

    private fun pop(): Long { // Pops a 16-bit value
        val currentSpOffset = cpu.readReg(Reg.SP)
        val ssBase = cpu.readReg(Reg.SS) // SS holds segment base
        val physicalStackAddress = (ssBase + currentSpOffset) and 0xFFFF

        val v = mem.read16(physicalStackAddress)
        val newSpOffset = (currentSpOffset + 2) and 0xFFFF // SP is 16-bit offset
        cpu.writeReg(Reg.SP, newSpOffset)
        return v
    }

    fun run(maxSteps: Int = 100000) {
        var steps = 0
        // CS:IP is the conceptual execution pointer. Here, IP is an index into the program list.
        // CS is not directly used for fetching, as program is a direct list.
        while (cpu.IP >= 0 && cpu.IP < program.size) { // Ensure IP is within bounds
            if (steps++ > maxSteps) error("Execution limit exceeded (possible infinite loop at IP=${cpu.IP})")
            val ins = program[cpu.IP.toInt()] // cpu.IP is Long, program.get expects Int
            // println("IP=${cpu.IP.toString(16).padStart(4,'0')} SS:SP=${readReg(Reg.SS).toString(16).padStart(4,'0')}:${readReg(Reg.SP).toString(16).padStart(4,'0')} Flags:[Z=${cpu.flags.ZF} S=${cpu.flags.SF} C=${cpu.flags.CF} O=${cpu.flags.OF}] $ins") // debug

            when (ins.op) {
                Op.NOP -> cpu.IP++
                Op.MOV -> {
                    val v = readOp(ins.src ?: error("MOV missing src at line ${ins.line}"), ins.line)
                    writeOp(ins.dst ?: error("MOV missing dst at line ${ins.line}"), v, ins.line)
                    cpu.IP++
                }
                Op.ADD -> {
                    val dst = ins.dst ?: error("ADD missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("ADD missing src at line ${ins.line}"), ins.line)
                    val r = (a + b) // Result before masking for flags
                    setFlagsFromAdd(a, b, r and 0xFFFF)
                    writeOp(dst, r and 0xFFFF, ins.line)
                    cpu.IP++
                }
                Op.SUB -> {
                    val dst = ins.dst ?: error("SUB missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("SUB missing src at line ${ins.line}"), ins.line)
                    val r = (a - b) // Result before masking for flags
                    setFlagsFromSub(a, b, r and 0xFFFF)
                    writeOp(dst, r and 0xFFFF, ins.line)
                    cpu.IP++
                }
                Op.INC -> {
                    val dst = ins.dst ?: error("INC missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = (a + 1)
                    setFlagsFromAdd(a, 1, r and 0xFFFF) // INC sets flags like ADD x, 1
                    writeOp(dst, r and 0xFFFF, ins.line)
                    cpu.IP++
                }
                Op.DEC -> {
                    val dst = ins.dst ?: error("DEC missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = (a - 1)
                    setFlagsFromSub(a, 1, r and 0xFFFF) // DEC sets flags like SUB x, 1
                    writeOp(dst, r and 0xFFFF, ins.line)
                    cpu.IP++
                }
                Op.CMP -> {
                    // CMP is like SUB but only sets flags, doesn't store result.
                    val a = readOp(ins.dst ?: error("CMP missing left operand at line ${ins.line}"), ins.line)
                    val b = readOp(ins.src ?: error("CMP missing right operand at line ${ins.line}"), ins.line)
                    val r = (a - b)
                    setFlagsFromSub(a, b, r and 0xFFFF)
                    cpu.IP++
                }
                Op.JMP, Op.JE, Op.JNE, Op.JG, Op.JL, Op.JGE, Op.JLE -> {
                    val targetOp = ins.dst ?: ins.src ?: error("Jump missing target operand at line ${ins.line}")
                    val take = (ins.op == Op.JMP) || jccTaken(ins.op)
                    if (take) {
                        val targetIdx = readOp(targetOp, ins.line)
                        // Jumps are to instruction indices (offsets)
                        if (targetIdx < 0 || targetIdx >= program.size) error("Jump target $targetIdx out of program range (0-${program.size-1}) at line ${ins.line}")
                        cpu.IP = targetIdx.toLong()
                    } else {
                        cpu.IP++
                    }
                }
                Op.PUSH -> {
                    val v = readOp(ins.dst ?: error("PUSH missing operand at line ${ins.line}"), ins.line)
                    push(v)
                    cpu.IP++
                }
                Op.POP -> {
                    val dst = ins.dst ?: error("POP missing destination at line ${ins.line}")
                    val v = pop()
                    writeOp(dst, v, ins.line)
                    cpu.IP++
                }
                Op.CALL -> {
                    val targetOp = ins.dst ?: error("CALL missing target at line ${ins.line}")
                    val target = readOp(targetOp, ins.line)
                    // Call target is an instruction index (offset)
                    if (target < 0 || target >= program.size) error("Call target $target out of program range (0-${program.size-1}) at line ${ins.line}")
                    push(cpu.IP + 1) // Push return address (offset)
                    cpu.IP = target.toLong()
                }
                Op.RET -> {
                    // Pop return address (offset)
                    val returnIp = pop()
                    if (returnIp < 0 || returnIp >= program.size) error("Return IP $returnIp out of program range (0-${program.size-1})")
                    cpu.IP = returnIp.toLong()
                }
                Op.INT -> {
                    val imm = readOp(ins.dst ?: error("INT requires an immediate at line ${ins.line}"), ins.line)
                    if ((imm and 0xFF) == 0x20L) { // INT 20h -> DOS terminate program
                        println("Program terminated via INT 20h (IP=${cpu.IP})")
                        return // Exit run loop
                    } else {
                        error("Unsupported interrupt 0x${imm.toString(16)} at line ${ins.line}")
                    }
                }
                // TODO: Implement other Opcodes from model.Op
                else -> error("Line ${ins.line}: Unimplemented opcode ${ins.op}")
            }
        }
        if (!(cpu.IP >= 0 && cpu.IP < program.size)) {
            println("Program halted. IP (${cpu.IP.toString(16)}) out of bounds (0-${(program.size-1).toString(16)}).")
        }
    }

    fun printRegisters() {
        cpu.printRegisters() // Delegate to CPU's more detailed printRegisters
    }

    fun printMemory() { // Kept for specific interpreter-level memory view if needed, or could also delegate/enhance
        println("Memory (showing first 256 bytes):") // Limit output for brevity
        for (i in 0 until 256 step 16) {
            print("${i.toString(16).padStart(4, '0')} | ")
            for (j in 0 until 16) {
                val physicalAddress = i + j
                if (physicalAddress < mem.size) { // Check bounds
                    val v = mem.read8(physicalAddress.toLong())
                    print("${v.toString(16).padStart(2, '0')} ")
                } else {
                    print("   ") // Out of bounds
                }
            }
            println()
        }
    }
}
