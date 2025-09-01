package asm

import cpu.CPU
import cpu.Memory
import model.Instruction
import model.Op
import model.Operand
import model.Reg

class Interpreter(
    private val program: List<Instruction>,
    private val labels: Map<String, Long>,
    private val mem: Memory
) {
    private val cpu = CPU()

    init {
        // Initialize Stack Pointer to the top of memory. SS is 0, so SP is effectively the physical address.
        // mem.size is Int, cpu.regs values are Long.
        cpu.writeReg(Reg.SP, mem.size.toLong()) // SP will be an offset; if SS is 0, it's also the physical top.
    }

    private fun addrOf(memOp: Operand.MemOp): Long {
        val offsetPart = (memOp.base?.let { cpu.readReg(it) } ?: 0) + (memOp.disp ?: 0)

        // Determine segment register: BP-based addressing uses SS, otherwise DS.
        val segmentReg = if (memOp.base == Reg.BP) Reg.SS else Reg.DS
        val segmentBase = cpu.readReg(segmentReg)

        // Physical address: (SegmentBase + Offset) & 0xFFFFFFFF (assuming 32-bit address space)
        // If segment registers are paragraph addresses (base * 16), this calculation would change.
        // For now, assume segment registers hold the linear base address.
        return (segmentBase + offsetPart) and 0xFFFFFFFFL
    }

    private fun readOp(op: Operand, line: Int, size: Int = 32): Long = when(op) {
        is Operand.ImmOp -> when (size) {
            8 -> op.value and 0xFFL
            16 -> op.value and 0xFFFFL
            32 -> op.value and 0xFFFFFFFFL // Ensure immediate is treated as 32-bit
            else -> error("Line $line: Unsupported size $size for immediate operand")
        }
        is Operand.RegOp -> cpu.readReg(op.reg) // CPU handles size for register reads
        is Operand.MemOp -> when (size) {
            8 -> mem.readByte(addrOf(op))
            16 -> mem.readWord(addrOf(op))
            32 -> mem.readDWord(addrOf(op))
            else -> error("Line $line: Unsupported size $size for memory read")
        }
        is Operand.LabelOp -> labels[op.name]?.let { it.toLong() and 0xFFFFFFFFL } ?: error("Line $line: Unknown label: ${op.name}")
        else -> error("Line $line: Unimplemented operand type ${op.javaClass.simpleName}")
    }

    private fun writeOp(op: Operand, value: Long, line: Int, size: Int = 32) {
        when(op) {
            is Operand.RegOp -> cpu.writeReg(op.reg, value) // CPU handles size for register writes
            is Operand.MemOp -> {
                val addr = addrOf(op)
                when (size) {
                    8 -> mem.writeByte(addr, value)
                    16 -> mem.writeWord(addr, value)
                    32 -> mem.writeDWord(addr, value)
                    else -> error("Line $line: Unsupported size $size for memory write")
                }
            }
            is Operand.ImmOp -> error("Line $line: cannot write to immediate")
            is Operand.LabelOp -> error("Line $line: cannot write to a label")
            else -> error("Line $line: Unimplemented operand type for write ${op.javaClass.simpleName}")
        }
    }

    private fun setFlagsFromAdd(a: Long, b: Long, res: Long) {
        val mask = 0xFFFFFFFFL
        val signBit = 0x80000000L

        cpu.flags.ZF = (res and mask) == 0L
        cpu.flags.SF = (res and signBit) != 0L

        val ua = a and mask
        val ub = b and mask
        cpu.flags.CF = (ua + ub) > mask

        val aSign = (a and signBit) != 0L
        val bSign = (b and signBit) != 0L
        val resSign = (res and signBit) != 0L
        cpu.flags.OF = (aSign == bSign) && (aSign != resSign)

        cpu.flags.AF = ((a and 0xFL) + (b and 0xFL)) > 0xFL

        var tempRes = res and 0xFFL
        var parity = 0
        while (tempRes > 0) {
            parity += (tempRes and 1L).toInt()
            tempRes = tempRes shr 1
        }
        cpu.flags.PF = (parity % 2) == 0
    }

    private fun setFlagsFromSub(a: Long, b: Long, res: Long) {
        val mask = 0xFFFFFFFFL
        val signBit = 0x80000000L

        cpu.flags.ZF = (res and mask) == 0L
        cpu.flags.SF = (res and signBit) != 0L
        cpu.flags.CF = (a and mask) < (b and mask)

        val aSign = (a and signBit) != 0L
        val bSign = (b and signBit) != 0L
        val resSign = (res and signBit) != 0L
        cpu.flags.OF = (aSign != bSign) && (bSign == resSign)
        
        cpu.flags.AF = (a and 0xFL) < (b and 0xFL)

        var tempRes = res and 0xFFL
        var parity = 0
        while (tempRes > 0) {
            parity += (tempRes and 1L).toInt()
            tempRes = tempRes shr 1
        }
        cpu.flags.PF = (parity % 2) == 0
    }


    private fun jccTaken(op: Op): Boolean = when(op) {
        Op.JE -> cpu.flags.ZF
        Op.JNE -> !cpu.flags.ZF
        Op.JG -> (cpu.flags.SF == cpu.flags.OF) && !cpu.flags.ZF
        Op.JL -> cpu.flags.SF != cpu.flags.OF
        Op.JGE -> cpu.flags.SF == cpu.flags.OF
        Op.JLE -> (cpu.flags.SF != cpu.flags.OF) || cpu.flags.ZF
        Op.JA -> !cpu.flags.CF && !cpu.flags.ZF
        Op.JB -> cpu.flags.CF
        Op.JAE -> !cpu.flags.CF
        Op.JBE -> cpu.flags.CF || cpu.flags.ZF
        else -> false
    }

    private fun push(v: Long) { // Pushes a 32-bit value
        val currentSpOffset = cpu.readReg(Reg.SP)
        val newSpOffset = (currentSpOffset - 4) and 0xFFFFFFFFL // SP is 32-bit offset
        cpu.writeReg(Reg.SP, newSpOffset)

        val ssBase = cpu.readReg(Reg.SS) // SS holds segment base
        val physicalStackAddress = (ssBase + newSpOffset) and 0xFFFFFFFFL
        mem.writeDWord(physicalStackAddress, v) // Write Double Word (32-bit)
    }

    private fun pop(): Long { // Pops a 32-bit value
        val currentSpOffset = cpu.readReg(Reg.SP)
        val ssBase = cpu.readReg(Reg.SS) // SS holds segment base
        val physicalStackAddress = (ssBase + currentSpOffset) and 0xFFFFFFFFL

        val v = mem.readDWord(physicalStackAddress) // Read Double Word (32-bit)
        val newSpOffset = (currentSpOffset + 4) and 0xFFFFFFFFL // SP is 32-bit offset
        cpu.writeReg(Reg.SP, newSpOffset)
        return v
    }

    fun run(maxSteps: Int = 100000) {
        var steps = 0
        while (cpu.IP >= 0 && cpu.IP < program.size) {
            if (steps++ > maxSteps) error("Execution limit exceeded (possible infinite loop at IP=${cpu.IP})")
            val ins = program[cpu.IP.toInt()]

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
                    val r = (a + b) 
                    setFlagsFromAdd(a, b, r and 0xFFFFFFFFL)
                    writeOp(dst, r and 0xFFFFFFFFL, ins.line)
                    cpu.IP++
                }
                Op.SUB -> {
                    val dst = ins.dst ?: error("SUB missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("SUB missing src at line ${ins.line}"), ins.line)
                    val r = (a - b) 
                    setFlagsFromSub(a, b, r and 0xFFFFFFFFL)
                    writeOp(dst, r and 0xFFFFFFFFL, ins.line)
                    cpu.IP++
                }
                Op.INC -> {
                    val dst = ins.dst ?: error("INC missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = (a + 1)
                    setFlagsFromAdd(a, 1, r and 0xFFFFFFFFL) 
                    writeOp(dst, r and 0xFFFFFFFFL, ins.line)
                    cpu.IP++
                }
                Op.DEC -> {
                    val dst = ins.dst ?: error("DEC missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = (a - 1)
                    setFlagsFromSub(a, 1, r and 0xFFFFFFFFL) 
                    writeOp(dst, r and 0xFFFFFFFFL, ins.line)
                    cpu.IP++
                }
                Op.CMP -> {
                    val a = readOp(ins.dst ?: error("CMP missing left operand at line ${ins.line}"), ins.line)
                    val b = readOp(ins.src ?: error("CMP missing right operand at line ${ins.line}"), ins.line)
                    val r = (a - b)
                    setFlagsFromSub(a, b, r and 0xFFFFFFFFL)
                    cpu.IP++
                }
                Op.JMP, Op.JE, Op.JNE, Op.JG, Op.JL, Op.JGE, Op.JLE, Op.JA, Op.JB, Op.JAE, Op.JBE -> { // Added other Jcc ops
                    val targetOp = ins.dst ?: ins.src ?: error("Jump missing target operand at line ${ins.line}")
                    val take = (ins.op == Op.JMP) || jccTaken(ins.op)
                    if (take) {
                        val targetIdx = readOp(targetOp, ins.line)
                        if (targetIdx < 0 || targetIdx >= program.size) error("Jump target $targetIdx out of program range (0-${program.size-1}) at line ${ins.line}")
                        cpu.IP = targetIdx.toLong()
                    } else {
                        cpu.IP++
                    }
                }
                Op.PUSH -> {
                    val v = readOp(ins.dst ?: error("PUSH missing operand at line ${ins.line}"), ins.line)
                    push(v and 0xFFFFFFFFL) // Ensure value pushed is 32-bit
                    cpu.IP++
                }
                Op.POP -> {
                    val dst = ins.dst ?: error("POP missing destination at line ${ins.line}")
                    val v = pop() // pop already returns 32-bit
                    writeOp(dst, v, ins.line) // writeOp will handle 32-bit write
                    cpu.IP++
                }
                Op.CALL -> {
                    val targetOp = ins.dst ?: error("CALL missing target at line ${ins.line}")
                    val target = readOp(targetOp, ins.line)
                    if (target < 0 || target >= program.size) error("Call target $target out of program range (0-${program.size-1}) at line ${ins.line}")
                    push((cpu.IP + 1) and 0xFFFFFFFFL) // Push return address (32-bit)
                    cpu.IP = target.toLong()
                }
                Op.RET -> {
                    val returnIp = pop() // pop returns 32-bit
                    if (returnIp < 0 || returnIp >= program.size) error("Return IP $returnIp out of program range (0-${program.size-1})")
                    cpu.IP = returnIp.toLong()
                }
                Op.INT -> {
                    val imm = readOp(ins.dst ?: error("INT requires an immediate at line ${ins.line}"), ins.line)
                    if ((imm and 0xFFL) == 0x20L) { 
                        println("Program terminated via INT 20h (IP=${cpu.IP})")
                        return 
                    } else {
                        error("Unsupported interrupt 0x${(imm and 0xFFFFFFFFL).toString(16)} at line ${ins.line}")
                    }
                }
                Op.AND -> {
                    val dst = ins.dst ?: error("AND missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("AND missing src at line ${ins.line}"), ins.line)
                    val r = a and b
                    writeOp(dst, r and 0xFFFFFFFFL, ins.line)
                    cpu.flags.ZF = (r and 0xFFFFFFFFL) == 0L
                    cpu.flags.SF = (r and 0x80000000L) != 0L
                    cpu.flags.CF = false 
                    cpu.flags.OF = false 
                    // PF and AF might need adjustment for AND/OR/XOR if specific behavior is desired,
                    // often they are undefined or set to specific values. For now, mirroring ADD/SUB's PF.
                    var tempRes = r and 0xFFL
                    var parity = 0
                    while (tempRes > 0) {
                        parity += (tempRes and 1L).toInt()
                        tempRes = tempRes shr 1
                    }
                    cpu.flags.PF = (parity % 2) == 0
                    cpu.flags.AF = false // Typically AF is undefined or cleared by logical ops
                    cpu.IP++
                }
                Op.OR -> {
                    val dst = ins.dst ?: error("OR missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("OR missing src at line ${ins.line}"), ins.line)
                    val r = a or b
                    writeOp(dst, r and 0xFFFFFFFFL, ins.line)
                    cpu.flags.ZF = (r and 0xFFFFFFFFL) == 0L
                    cpu.flags.SF = (r and 0x80000000L) != 0L
                    cpu.flags.CF = false 
                    cpu.flags.OF = false 
                    var tempRes = r and 0xFFL
                    var parity = 0
                    while (tempRes > 0) {
                        parity += (tempRes and 1L).toInt()
                        tempRes = tempRes shr 1
                    }
                    cpu.flags.PF = (parity % 2) == 0
                    cpu.flags.AF = false
                    cpu.IP++
                }
                Op.XOR -> {
                    val dst = ins.dst ?: error("XOR missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("XOR missing src at line ${ins.line}"), ins.line)
                    val r = a xor b
                    writeOp(dst, r and 0xFFFFFFFFL, ins.line)
                    cpu.flags.ZF = (r and 0xFFFFFFFFL) == 0L
                    cpu.flags.SF = (r and 0x80000000L) != 0L
                    cpu.flags.CF = false 
                    cpu.flags.OF = false 
                    var tempRes = r and 0xFFL
                    var parity = 0
                    while (tempRes > 0) {
                        parity += (tempRes and 1L).toInt()
                        tempRes = tempRes shr 1
                    }
                    cpu.flags.PF = (parity % 2) == 0
                    cpu.flags.AF = false
                    cpu.IP++
                }
                Op.NOT -> {
                    val dst = ins.dst ?: error("NOT missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = a.inv() 
                    writeOp(dst, r and 0xFFFFFFFFL, ins.line)
                    // NOT does not affect flags
                    cpu.IP++
                }
                else -> error("Line ${ins.line}: Unimplemented opcode ${ins.op}")
            }
        }
        if (!(cpu.IP >= 0 && cpu.IP < program.size)) {
            println("Program halted. IP (${(cpu.IP and 0xFFFFFFFFL).toString(16)}) out of bounds (0-${(program.size-1).toString(16)}).")
        }
    }

    fun printRegisters() {
        cpu.printRegisters()
    }

    fun printMemory() { 
        println("Memory (showing first 256 bytes):")
        for (i in 0 until 256 step 16) {
            print("${i.toString(16).padStart(4, '0')} | ")
            for (j in 0 until 16) {
                val physicalAddress = i + j
                if (physicalAddress < mem.size) { 
                    val v = mem.readByte(physicalAddress.toLong())
                    print("${v.toString(16).padStart(2, '0')} ")
                } else {
                    print("   ") 
                }
            }
            println()
        }
    }
}
