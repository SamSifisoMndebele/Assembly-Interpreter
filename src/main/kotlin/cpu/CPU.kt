package cpu

import model.EFlags
import model.Instruction
import model.Operation
import model.Operand
import model.Operand.ImmOp
import model.Operand.LabelOp
import model.Operand.MemOp
import model.Operand.RegOp
import model.Reg
import model.Reg.*

@OptIn(ExperimentalUnsignedTypes::class)
class CPU(private val mem: Memory) {
    private val symbolTable = mutableMapOf<String, UInt>()

    private enum class Reg32 { EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP }
    private val regs = UIntArray(8)
    @Suppress("PrivatePropertyName")
    private var EFLAGS: UInt = 0u
    @Suppress("PrivatePropertyName")
    private var IP: UInt = 0u
    private enum class SegReg { CS, DS, SS, ES, FS, GS }
    private val segRegs = UShortArray(6)

    private val Reg.reg32: Int get() = when (this) {
        EAX, AX, AL, AH -> Reg32.EAX.ordinal
        EBX, BX, BL, BH -> Reg32.EBX.ordinal
        ECX, CX, CL, CH -> Reg32.ECX.ordinal
        EDX, DX, DL, DH -> Reg32.EDX.ordinal
        ESI, SI -> Reg32.ESI.ordinal
        EDI, DI -> Reg32.EDI.ordinal
        EBP, BP -> Reg32.EBP.ordinal
        ESP, SP -> Reg32.ESP.ordinal
        else -> error("Unsupported register: $this")
    }

    // === Register access ===
    private fun get32(reg: Reg) = regs[reg.reg32]
    private fun set32(reg: Reg, value: UInt) { regs[reg.reg32] = value }
    private fun get16(reg: Reg) = (regs[reg.reg32] and 0xFFFFu).toUShort()
    private fun set16(reg: Reg, value: UShort) {
        val i = reg.reg32
        regs[i] = (regs[i] and 0xFFFF_0000u) or (value.toUInt() and 0xFFFFu)
    }
    private fun get8(reg: Reg): UByte {
        val raw = regs[reg.reg32]
        return when (reg) {
            AL, BL, CL, DL -> (raw and 0xFFu).toUByte()
            AH, BH, CH, DH -> ((raw shr 8) and 0xFFu).toUByte()
            else -> error("Unsupported register: $reg")
        }
    }
    private fun set8(reg: Reg, value: UByte) {
        val i = reg.reg32
        val raw = regs[i]
        regs[i] = when (reg) {
            AL, BL, CL, DL -> (raw and 0xFFFF_FF00u) or value.toUInt()
            AH, BH, CH, DH -> (raw and 0xFFFF_00FFu) or (value.toUInt() shl  8)
            else -> error("Unsupported register: $reg")
        }
    }
    private fun push(value: UInt) {
        val esp = regs[Reg32.ESP.ordinal]
        regs[Reg32.ESP.ordinal] = esp - 4u
        mem.writeDWord((esp - 4u).toInt(), value)
    }
    private fun pop(): UInt {
        val esp = regs[Reg32.ESP.ordinal]
        val value = mem.readDWord(esp.toInt())
        regs[Reg32.ESP.ordinal] = esp + 4u
        return value
    }
    private fun getSeg(seg: SegReg) = segRegs[seg.ordinal]
    private fun setSeg(seg: SegReg, value: UShort) {
        segRegs[seg.ordinal] = value
    }
    private fun setFlag(flag: EFlags, state: Boolean) {
        EFLAGS = if (state) {
            EFLAGS or (1u shl flag.bit)
        } else {
            EFLAGS and (1u shl flag.bit).inv()
        }
    }
    private fun getFlag(flag: EFlags): Boolean {
        return (EFLAGS and (1u shl flag.bit)) != 0u
    }


    // === Helpers ===
    private fun parity(b: UByte) = Integer.bitCount(b.toInt()) % 2 == 0
    private fun updateZN(v: UInt) {
        setFlag(EFlags.ZF, v == 0u)
        setFlag(EFlags.SF, (v shr 31) == 1u)
        setFlag(EFlags.PF, parity((v and 0xFFu).toUByte()))
    }
    private fun addFlags(a: UInt, b: UInt, res: UInt) {
        setFlag(EFlags.CF, res < a)
        val sa = (a shr 31) and 1u
        val sb = (b shr 31) and 1u
        val sr = (res shr 31) and 1u
        setFlag(EFlags.OF, (sa == sb && sa != sr))
        setFlag(EFlags.AF, (((a and 0xFu) + (b and 0xFu)) and 0x10u) != 0u)
        updateZN(res)
    }
    private fun subFlags(a: UInt, b: UInt, res: UInt) {
        setFlag(EFlags.CF, a < b)
        val sa = (a shr 31) and 1u
        val sb = (b shr 31) and 1u
        val sr = (res shr 31) and 1u
        setFlag(EFlags.OF, (sa != sb && sr != sa))
        setFlag(EFlags.AF, (((a and 0xFu) - (b and 0xFu)) and 0x10u) != 0u)
        updateZN(res)
    }
    private fun segAddr(seg: UShort, offset: UInt): UInt {
        return (seg.toUInt() shl 4) + (offset and 0xFFFFu)
    }


    // === Operand access ===
    private fun read(op: Operand): UInt = when (op) {
        is ImmOp -> op.value
        is RegOp -> when (op.reg) {
            AL, AH, BL, BH, CL, CH, DL, DH -> get8(op.reg).toUInt()
            AX, BX, CX, DX, SI, DI, BP, SP -> get16(op.reg).toUInt()
            else -> get32(op.reg)
        }
        is MemOp -> {
            val base = op.base?.let { get32(it) } ?: 0u
            val disp = op.disp ?: 0u
            val addr = base + disp
            mem.readDWord(addr.toInt())
        }
        is LabelOp -> error("Label must be resolved before execution")
    }
    private fun write(op: Operand, value: UInt) {
        when (op) {
            is RegOp -> when (op.reg) {
                AL, AH, BL, BH, CL, CH, DL, DH -> set8(op.reg, (value and 0xFFu).toUByte())
                AX, BX, CX, DX, SI, DI, BP, SP -> set16(op.reg, (value and 0xFFFFu).toUShort())
                else -> set32(op.reg, value)
            }
            is MemOp -> {
                val base = op.base?.let { get32(it) } ?: 0u
                val disp = op.disp ?: 0u
                val addr = base + disp
                mem.writeDWord(addr.toInt(), value)
            }
            else -> error("Cannot write to $op")
        }
    }

    init {
        regs.fill(0u)
        regs[Reg32.ESP.ordinal] = mem.size.toUInt()
    }

    // === Execution ===
    fun execute(instruction: Instruction): Boolean {
        when(instruction) {
            is Instruction.InstructionZero -> {
                when(instruction.operation) {
                    Operation.OperationZero.NOP -> {}
                    Operation.OperationZero.POPA -> {
                        set32(Reg.EDI, pop())
                        set32(Reg.ESI, pop())
                        set32(Reg.EBP, pop())
                        pop() // Skip ESP
                        set32(Reg.EBX, pop())
                        set32(Reg.EDX, pop())
                        set32(Reg.ECX, pop())
                        set32(Reg.EAX, pop())
                    }
                    Operation.OperationZero.PUSHA -> {
                        val tempEsp = get32(Reg.ESP)
                        push(get32(Reg.EAX))
                        push(get32(Reg.ECX))
                        push(get32(Reg.EDX))
                        push(get32(Reg.EBX))
                        push(tempEsp)
                        push(get32(Reg.EBP))
                        push(get32(Reg.ESI))
                        push(get32(Reg.EDI))
                    }
                }
            }
            is Instruction.InstructionOne -> {
                val operand = instruction.operand
                when(instruction.operation) {
                    Operation.OperationOne.DEC -> {
                        val a = read(operand)
                        val res = a - 1u
                        write(operand, res)
                        updateZN(res)
                        val sa = (a shr 31) and 1u
                        val sr = (res shr 31) and 1u
                        setFlag(EFlags.OF, sa == 1u && sr == 0u)
                        setFlag(EFlags.AF, ((a and 0xFu) - 1u) and 0x10u != 0u)
                    }
                    Operation.OperationOne.INC -> {
                        val a = read(operand)
                        val res = a + 1u
                        write(operand, res)
                        updateZN(res)
                        val sa = (a shr 31) and 1u
                        val sr = (res shr 31) and 1u
                        setFlag(EFlags.OF, sa == 0u && sr == 1u)
                        setFlag(EFlags.AF, ((a and 0xFu) + 1u) and 0x10u != 0u)
                    }
                    Operation.OperationOne.POP -> {
                        write(operand, pop())
                    }
                    Operation.OperationOne.PUSH -> {
                        push(read(operand))
                    }
//                    Operation.OperationOne.CALL -> {
//                        if (operand !is ImmOp) error("CALL requires immediate destination")
//                        push(IP)
//                        IP = operand.value
//                        return true
//                    }
                }
            }
            is Instruction.InstructionTwo -> {
                val dst = instruction.dst
                val src = instruction.src
                when(instruction.operation) {
                    Operation.OperationTwo.ADD -> {
                        val a = read(dst)
                        val b = read(src)
                        val res = a + b
                        write(dst, res)
                        addFlags(a, b, res)
                    }
                    Operation.OperationTwo.MOV -> {
                        write(dst, read(src))
                        updateZN(read(dst))
                    }
                    Operation.OperationTwo.SUB -> {
                        val a = read(dst)
                        val b = read(src)
                        val res = a - b
                        write(dst, res)
                        subFlags(a, b, res)
                    }
                    Operation.OperationTwo.XCHG -> {
                        val val1 = read(dst)
                        val val2 = read(src)
                        write(dst, val2)
                        write(src, val1)
                    }
                }
            }
            is Instruction.LabelDefinition -> {
                val operand = instruction.operand
                val targetIp = when(operand) {
                    is LabelOp -> error("Label ${operand.label} must be resolved to an address before execution")
                    is ImmOp -> operand.value
                    else -> error("Jump/Loop target must be an immediate value (address)")
                }
                when (instruction.label) {
                    Operation.OperationLabel.JMP -> {
                        IP = targetIp
                        return true // Do not increment IP after jump
                    }
                    Operation.OperationLabel.JNZ -> {
                        if (!getFlag(EFlags.ZF)) {
                            IP = targetIp
                            return true // Do not increment IP after jump
                        }
                    }
                    Operation.OperationLabel.JZ -> {
                        if (getFlag(EFlags.ZF)) {
                            IP = targetIp
                            return true // Do not increment IP after jump
                        }
                    }
                    Operation.OperationLabel.LOOP -> {
                        val ecx = get32(Reg.ECX) - 1u
                        set32(Reg.ECX, ecx)
                        if (ecx != 0u) {
                            IP = targetIp
                            return true // Do not increment IP after jump
                        }
                    }
                }
            }
        }

        IP += 1u
        return false
    }

    fun run(prog: List<Instruction>, start: Int=0, max: Int=1000) {
        var pc=start
        var steps=0
        while (pc in prog.indices && steps<max) {
            val ins=prog[pc]
            val cont=execute(ins)
            if(!cont) break
            pc++
            steps++
        }
    }

    // === Debug helpers ===
    fun get(r: Reg): UInt = when (r) {
        Reg.AL,Reg.AH,Reg.BL,Reg.BH,Reg.CL,Reg.CH,Reg.DL,Reg.DH -> get8(r).toUInt()
        Reg.AX,Reg.BX,Reg.CX,Reg.DX,Reg.SI,Reg.DI,Reg.BP,Reg.SP -> get16(r).toUInt()
        else -> get32(r)
    }
    fun set(r: Reg,v:UInt) { write(RegOp(r),v) }

    /**
     * Generates a string representation of the current state of the CPU registers and flags.
     */
    fun printRegisters() {
        val sb = StringBuilder()
        sb.appendLine("Registers Dump:")
        sb.appendLine("  EAX: 0x%08X (AX: 0x%04X, AH: 0x%02X, AL: 0x%02X)".format(get32(EAX).toInt(), get16(AX).toInt(), get8(AH).toInt(), get8(AL).toInt()))
        sb.appendLine("  EBX: 0x%08X (BX: 0x%04X, BH: 0x%02X, BL: 0x%02X)".format(get32(EBX).toInt(), get16(BX).toInt(), get8(BH).toInt(), get8(BL).toInt()))
        sb.appendLine("  ECX: 0x%08X (CX: 0x%04X, CH: 0x%02X, CL: 0x%02X)".format(get32(ECX).toInt(), get16(CX).toInt(), get8(CH).toInt(), get8(CL).toInt()))
        sb.appendLine("  EDX: 0x%08X (DX: 0x%04X, DH: 0x%02X, DL: 0x%02X)".format(get32(EDX).toInt(), get16(DX).toInt(), get8(DH).toInt(), get8(DL).toInt()))
        sb.appendLine("  ESI: 0x%08X (SI: 0x%04X)".format(get32(ESI).toInt(), get16(SI).toInt()))
        sb.appendLine("  EDI: 0x%08X (DI: 0x%04X)".format(get32(EDI).toInt(), get16(DI).toInt()))
        sb.appendLine("  EBP: 0x%08X (BP: 0x%04X)".format(get32(EBP).toInt(), get16(BP).toInt()))
        sb.appendLine("  ESP: 0x%08X (SP: 0x%04X)".format(get32(ESP).toInt(), get16(SP).toInt()))
        sb.appendLine("  IP:  0x%08X".format(IP.toInt()))
        sb.appendLine("Segment Registers:")
        sb.appendLine("  CS: 0x%04X   DS: 0x%04X   SS: 0x%04X   ES: 0x%04X".format(getSeg(SegReg.CS).toInt(), getSeg(SegReg.DS).toInt(), getSeg(SegReg.SS).toInt(), getSeg(SegReg.ES).toInt()))
        sb.appendLine("Flags: CF=${getFlag(EFlags.CF).int} PF=${getFlag(EFlags.PF).int} AF=${getFlag(EFlags.AF).int} ZF=${getFlag(EFlags.ZF).int} SF=${getFlag(EFlags.SF).int} OF=${getFlag(EFlags.OF).int}")
        println(sb.toString())
    }
    private val Boolean.int get() = if (this) 1 else 0
}

fun main() {
    val mem = Memory(64f / (1024*1024))
    val cpu = CPU(mem)
    println("Memory size: ${mem.size} bytes")

//    val program = listOf(
//        Instruction(Operation.MOV, RegOp(EAX), ImmOp(4u)),
//        Instruction(Operation.PUSH, RegOp(EAX)),
//        Instruction(Operation.PUSH, RegOp(AX)),
//        Instruction(Operation.MOV, RegOp(EAX), ImmOp(5u)),
//        Instruction(Operation.MOV, RegOp(EBX), ImmOp(7u)),
//        Instruction(Operation.ADD, RegOp(EAX), RegOp(EBX)),
//        Instruction(Operation.MOV, RegOp(ECX), RegOp(EAX)),
////        Instruction(Op.POP, RegOp(EAX)),
//    )
//
//    cpu.run(program)
//
//    println("EAX = ${cpu.get(EAX)}") // should print 12
//    println("ZF=${cpu.getFlags().ZF} SF=${cpu.getFlags().SF}")


    cpu.printRegisters()
    mem.printMemory()

}