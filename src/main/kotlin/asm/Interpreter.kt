package asm

import cpu.CPU
import cpu.Memory
import model.Instruction
import model.Op
import model.Operand
import model.Reg

class Interpreter(private val program: List<Instruction>, private val labels: Map<String, Int>, private val mem: Memory) {
    private val cpu = CPU()

    init {
        // Initialize Stack Pointer to the top of memory.
        cpu.regs[Reg.SP] = mem.size and 0xFFFFFFFFFFFF
    }

    private fun readReg(r: Reg) = cpu.regs[r]!! and 0xFFFF
    private fun writeReg(r: Reg, v: Int) { cpu.regs[r] = v and 0xFFFF }

    private fun addrOf(memOp: Operand.MemOp): Int {
        val base = memOp.base?.let { readReg(it) } ?: 0
        val disp = memOp.disp ?: 0
        return (base + disp) and 0xFFFF
    }

    private fun readOp(op: Operand, line: Int): Int = when(op) {
        is Operand.ImmOp -> op.value and 0xFFFF
        is Operand.RegOp -> readReg(op.reg)
        is Operand.MemOp -> mem.read16(addrOf(op))
        is Operand.LabelOp -> labels[op.name]?.let { it and 0xFFFF } ?: error("Line $line: Unknown label: ${op.name}")
    }

    private fun writeOp(op: Operand, value: Int, line: Int) {
        when(op) {
            is Operand.RegOp -> writeReg(op.reg, value)
            is Operand.MemOp -> mem.write16(addrOf(op), value)
            is Operand.ImmOp -> error("Line $line: cannot write to immediate")
            is Operand.LabelOp -> error("Line $line: cannot write to a label")
        }
    }

    private fun setFlagsFromAdd(a: Int, b: Int, res: Int) { // a, b are 16-bit unsigned-like values
        cpu.flags.ZF = (res and 0xFFFF) == 0
        cpu.flags.SF = (res and 0x8000) != 0
        cpu.flags.CF = (a + b) > 0xFFFF
    }

    private fun setFlagsFromSub(a: Int, b: Int, res: Int) {
        cpu.flags.ZF = (res and 0xFFFF) == 0
        cpu.flags.SF = (res and 0x8000) != 0
        cpu.flags.CF = (a - b) < 0
    }

    private fun jccTaken(op: Op): Boolean = when(op) {
        Op.JE -> cpu.flags.ZF
        Op.JNE -> !cpu.flags.ZF
        Op.JG -> (!cpu.flags.SF && !cpu.flags.ZF)
        Op.JL -> cpu.flags.SF
        Op.JGE -> !cpu.flags.SF
        Op.JLE -> (cpu.flags.SF || cpu.flags.ZF)
        else -> false
    }

    private fun push(v: Int) {
        val newSp = (readReg(Reg.SP) - 2) and 0xFFFF
        writeReg(Reg.SP, newSp)
        mem.write16(newSp, v)
    }

    private fun pop(): Int {
        val sp = readReg(Reg.SP)
        val v = mem.read16(sp)
        writeReg(Reg.SP, (sp + 2) and 0xFFFF)
        return v
    }

    fun run(maxSteps: Int = 100000) {
        var steps = 0
        while (cpu.IP in program.indices) {
            if (steps++ > maxSteps) error("Execution limit exceeded (possible infinite loop)")
            val ins = program[cpu.IP]
            // println("IP=${cpu.IP} ${ins}") // debug
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
                    val r = (a + b) and 0xFFFF
                    setFlagsFromAdd(a, b, r)
                    writeOp(dst, r, ins.line)
                    cpu.IP++
                }
                Op.SUB -> {
                    val dst = ins.dst ?: error("SUB missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("SUB missing src at line ${ins.line}"), ins.line)
                    val r = (a - b) and 0xFFFF
                    setFlagsFromSub(a, b, r)
                    writeOp(dst, r, ins.line)
                    cpu.IP++
                }
                Op.INC -> {
                    val dst = ins.dst ?: error("INC missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = (a + 1) and 0xFFFF
                    setFlagsFromAdd(a, 1, r)
                    writeOp(dst, r, ins.line)
                    cpu.IP++
                }
                Op.DEC -> {
                    val dst = ins.dst ?: error("DEC missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = (a - 1) and 0xFFFF
                    setFlagsFromSub(a, 1, r)
                    writeOp(dst, r, ins.line)
                    cpu.IP++
                }
                Op.CMP -> {
                    val a = readOp(ins.dst ?: error("CMP missing left operand at line ${ins.line}"), ins.line)
                    val b = readOp(ins.src ?: error("CMP missing right operand at line ${ins.line}"), ins.line)
                    val r = (a - b) and 0xFFFF
                    setFlagsFromSub(a, b, r)
                    cpu.IP++
                }
                Op.JMP, Op.JE, Op.JNE, Op.JG, Op.JL, Op.JGE, Op.JLE -> {
                    val targetOp = ins.dst ?: ins.src ?: error("Jump missing target operand at line ${ins.line}")
                    val take = (ins.op == Op.JMP) || jccTaken(ins.op)
                    if (take) {
                        val targetIdx = readOp(targetOp, ins.line)
                        if (targetIdx !in program.indices) error("Jump target $targetIdx out of range at line ${ins.line}")
                        cpu.IP = targetIdx
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
                    if (target !in program.indices) error("Call target $target out of range at line ${ins.line}")
                    push(cpu.IP + 1)
                    cpu.IP = target
                }
                Op.RET -> {
                    cpu.IP = pop()
                }
                Op.INT -> {
                    val imm = readOp(ins.dst ?: error("INT requires an immediate at line ${ins.line}"), ins.line)
                    if ((imm and 0xFF) == 0x20) {
                        // INT 20h -> terminate
                        // println("Program terminated via INT 20h")
                        return
                    } else {
                        error("Unsupported interrupt $imm at line ${ins.line}")
                    }
                }
            }
        }
    }

    fun printRegisters() {
        println("CPU Registers:")
        cpu.regs.forEach { (r, v) -> println("$r: $v") }
    }

    fun printMemory() {
        println("Memory:")
        for (i in 0 until 0x10000 step 16) {
            print("${i.toString(16).padStart(4, '0')} | ")
            for (j in 0 until 16) {
                val v = mem.read8(i + j)
                print("${v.toString(16).padStart(2, '0')} ")
            }
            println()
        }
    }
}