package cpu

import model.EFlags
import model.Instruction
import model.Operation
import model.Operand
import model.Operand.*
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
    private var IP: UInt = 0u // Instruction Pointer
    private enum class SegReg { CS, DS, SS, ES, FS, GS }
    private val segRegs = UShortArray(6)

    // Method to define a symbol (label) and its address
    fun defineSymbol(label: String, address: UInt) {
        symbolTable[label] = address
    }

    private val Reg.reg32: Int
        get() = when (this) {
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
            AH, BH, CH, DH -> (raw and 0xFFFF_00FFu) or (value.toUInt() shl 8)
            else -> error("Unsupported register: $reg")
        }
    }
    private fun push(value: UInt) {
        val esp = regs[Reg32.ESP.ordinal] - 4u
        if (esp < 0u) error("Stack overflow: ESP offset would be negative.")
        regs[Reg32.ESP.ordinal] = esp
        // Use SS:ESP for stack operations
        val physicalAddress = segAddr(getSeg(SegReg.SS), esp)
        // Boundary check for the write operation
        if (physicalAddress >= mem.size.toUInt() || physicalAddress + 3u >= mem.size.toUInt()) {
            error("Stack physical write out of bounds: Addr=0x${physicalAddress.toString(16)}, ESP_Offset=0x${esp.toString(16)}, SS=0x${getSeg(SegReg.SS).toString(16)}, MemSize=0x${mem.size.toString(16)}")
        }
        mem.writeDWord(physicalAddress.toInt(), value)
    }
    private fun pop(): UInt {
        val esp = regs[Reg32.ESP.ordinal]
        // Use SS:ESP for stack operations
        val physicalAddress = segAddr(getSeg(SegReg.SS), esp)
        // Boundary check for the read operation
        // Assuming stackOffsetForEspInitialization holds the original top offset
        if (physicalAddress >= mem.size.toUInt() || physicalAddress + 3u >= mem.size.toUInt()) {
            error("Stack physical read out of bounds: Addr=0x${physicalAddress.toString(16)}, ESP_Offset=0x${esp.toString(16)}, SS=0x${getSeg(SegReg.SS).toString(16)}, MemSize=0x${mem.size.toString(16)}")
        }
        val value = mem.readDWord(physicalAddress.toInt())
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
    // Calculates physical address: (segment << 4) + offset
    private fun segAddr(segValue: UShort, offset: UInt): UInt {
        return (segValue.toUInt() shl 4) + offset
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
            val baseValue = op.base?.let { get32(it) } ?: 0u
            val displacement = op.disp ?: 0u
            val effectiveAddress = baseValue + displacement
            // Default to DS segment for memory access
            // TODO: Allow segment overrides for memory operands
            val physicalAddress = segAddr(getSeg(SegReg.DS), effectiveAddress)
            if (physicalAddress >= mem.size.toUInt() || physicalAddress + 3u >= mem.size.toUInt()) { // Assuming DWORD read
                error("Memory read out of bounds: Addr=0x${physicalAddress.toString(16)}, EA=0x${effectiveAddress.toString(16)}, DS=0x${getSeg(SegReg.DS).toString(16)}, MemSize=0x${mem.size.toString(16)}")
            }
            mem.readDWord(physicalAddress.toInt())
        }
        is LabelOp -> symbolTable[op.name] ?: error("Undefined label: ${op.name}")
    }
    private fun write(op: Operand, value: UInt) {
        when (op) {
            is RegOp -> when (op.reg) {
                AL, AH, BL, BH, CL, CH, DL, DH -> set8(op.reg, (value and 0xFFu).toUByte())
                AX, BX, CX, DX, SI, DI, BP, SP -> set16(op.reg, (value and 0xFFFFu).toUShort())
                else -> set32(op.reg, value)
            }
            is MemOp -> {
                val baseValue = op.base?.let { get32(it) } ?: 0u
                val displacement = op.disp ?: 0u
                val effectiveAddress = baseValue + displacement
                // Default to DS segment for memory access
                // TODO: Allow segment overrides for memory operands
                val physicalAddress = segAddr(getSeg(SegReg.DS), effectiveAddress)
                if (physicalAddress >= mem.size.toUInt() || physicalAddress + 3u >= mem.size.toUInt()) { // Assuming DWORD write
                    error("Memory write out of bounds: Addr=0x${physicalAddress.toString(16)}, EA=0x${effectiveAddress.toString(16)}, DS=0x${getSeg(SegReg.DS).toString(16)}, MemSize=0x${mem.size.toString(16)}")
                }
                mem.writeDWord(physicalAddress.toInt(), value)
            }
            else -> error("Cannot write to $op")
        }
    }

    // Store the initial ESP offset for stack boundary checks if needed
    private var stackOffsetForEspInitialization: UInt = 0u

    init {
        regs.fill(0u)
        segRegs.fill(0u)

        // Define memory layout constants
        val csBaseSegment: UShort = 0x0100u // Arbitrary CS base segment
        val dsBaseSegment: UShort = 0x1000u // Arbitrary DS base segment (e.g., physical 0x10000)
        val ssBaseSegment: UShort = 0x2000u // Arbitrary SS base segment (e.g., physical 0x20000)

        // Define stack size (e.g., 64KB)
        val stackSizeBytes = 0x10000u // 64KB

        // Calculate physical base for stack segment
        val stackPhysicalBase = ssBaseSegment.toUInt() shl 4

        // Calculate the highest physical address for the stack
        // ESP will be an offset from the start of the SS segment.
        // The stack grows downwards, so ESP is initialized to the size of the stack area.
        stackOffsetForEspInitialization = stackSizeBytes

        // Ensure the entire stack segment fits within the total available memory
        val stackPhysicalTop = stackPhysicalBase + stackOffsetForEspInitialization // This is one byte BEYOND the stack
        if (stackPhysicalTop > mem.size.toUInt()) {
            error("Stack segment (SS_base=0x${ssBaseSegment.toString(16)}, size=0x${stackSizeBytes.toString(16)}) exceeds available memory (0x${mem.size.toString(16)}). Max physical stack top: 0x${stackPhysicalTop.toString(16)}")
        }
        if (stackPhysicalBase >= mem.size.toUInt()) {
            error("Stack segment base (0x${stackPhysicalBase.toString(16)}) is outside memory (0x${mem.size.toString(16)})")
        }


        // Initialize Segment Registers
        setSeg(SegReg.CS, csBaseSegment)
        setSeg(SegReg.DS, dsBaseSegment)
        setSeg(SegReg.SS, ssBaseSegment)
        // ES, FS, GS could also be initialized, e.g., to DS or another segment.
        setSeg(SegReg.ES, dsBaseSegment)
        setSeg(SegReg.FS, 0u) // Typically 0 or some OS specific value
        setSeg(SegReg.GS, 0u) // Typically 0 or some OS specific value


        // Initialize ESP. ESP holds the offset *within the SS segment*.
        // If stack grows downwards, it's initialized to the size of the stack memory region.
        regs[Reg32.ESP.ordinal] = stackOffsetForEspInitialization

        // Sanity check after initialization
        val initialPhysicalEsp = segAddr(getSeg(SegReg.SS), get32(ESP))
        println("Initial SS:0x${getSeg(SegReg.SS).toString(16)}, ESP_offset:0x${get32(ESP).toString(16)}, Physical ESP (top of stack): 0x${initialPhysicalEsp.toString(16)}")
        println("Stack physical base: 0x${stackPhysicalBase.toString(16)}")
        println("Stack will operate between physical addresses 0x${stackPhysicalBase.toString(16)} and 0x${(stackPhysicalBase + stackSizeBytes - 1u).toString(16)} (inclusive)")

        // Check if the initial physical address pointed to by SS:ESP (before first push) is valid
        // Note: ESP points to the "top" free location. The first push will decrement ESP THEN write.
        // So, segAddr(SS, ESP-4) is the first address written to.
        if (stackOffsetForEspInitialization < 4u) {
            println("Warning: Initial stack size is very small, may lead to immediate overflow on push.")
        } else {
            val firstPushAddress = segAddr(getSeg(SegReg.SS), stackOffsetForEspInitialization - 4u)
            if (firstPushAddress < stackPhysicalBase || firstPushAddress >= mem.size.toUInt()) {
                error("Initial ESP setup results in first push (0x${firstPushAddress.toString(16)}) being out of stack segment or memory bounds.")
            }
        }
    }

    /**
     * Executes a single instruction.
     * @return true if IP was modified by a jump/call/ret, false otherwise.
     */
    fun execute(instruction: Instruction): Boolean {
        when(instruction) {
            is Instruction.InstructionZero -> {
                when(instruction.operation) {
                    Operation.OperationZero.NOP -> { /* Do nothing */ }
                    Operation.OperationZero.POPA -> {
                        set32(EDI, pop())
                        set32(ESI, pop())
                        set32(EBP, pop())
                        pop() // Skip ESP - this pops the pushed ESP value and discards it
                        set32(EBX, pop())
                        set32(EDX, pop())
                        set32(ECX, pop())
                        set32(EAX, pop())
                    }
                    Operation.OperationZero.PUSHA -> {
                        val tempEsp = get32(ESP) // ESP value *before* any registers are pushed by PUSHA
                        push(get32(EAX))
                        push(get32(ECX))
                        push(get32(EDX))
                        push(get32(EBX))
                        push(tempEsp) // Push original ESP
                        push(get32(EBP))
                        push(get32(ESI))
                        push(get32(EDI))
                    }
                     Operation.OperationZero.RET -> {
                        IP = pop() // Pop return address from stack into IP
                        return true // IP was modified
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
                        updateZN(res) // Z,S,P
                        // OF for DEC: set if positive operand becomes max negative, or min negative becomes positive.
                        // For a single decrement, OF is set if operand was 0x80000000 and becomes 0x7FFFFFFF.
                        // Or, more simply, if sign bit changes from 0 to 1 unexpectedly for result.
                        // Simplified: if positive number becomes negative (e.g. 0 -> -1 is not an overflow, 1 -> 0 is not)
                        // OF is set if the most significant bit of the operand was 0 and the most significant bit of the result is 1
                        // and the operand was not 0. (this needs more careful thought for exact x86 flags)
                        // A simpler OF for DEC x: set if x was min_int (0x80000000), result wraps to max_int (0x7FFFFFFF) - no, this is not correct for DEC.
                        // OF for DEC is set if result is 0x7FFFFFFF (max positive)
                        setFlag(EFlags.OF, res == 0x7FFFFFFFu) // Simplified, needs review for exact x86 behavior
                        setFlag(EFlags.AF, ((a and 0xFu) - 1u) and 0x10u != 0u)
                    }
                    Operation.OperationOne.INC -> {
                        val a = read(operand)
                        val res = a + 1u
                        write(operand, res)
                        updateZN(res) // Z,S,P
                        // OF for INC: set if result is 0x80000000 (min negative)
                        setFlag(EFlags.OF, res == 0x80000000u) // Simplified, needs review
                        setFlag(EFlags.AF, ((a and 0xFu) + 1u) and 0x10u != 0u)
                    }
                    Operation.OperationOne.POP -> {
                        write(operand, pop())
                    }
                    Operation.OperationOne.PUSH -> {
                        push(read(operand))
                    }
                    Operation.OperationOne.CALL -> {
                        val targetAddress = read(operand)
                        // Assuming IP points to the current CALL instruction.
                        // The return address is the address of the *next* instruction.
                        // If your instructions are 1 "unit" long in the program list:
                        push(IP + 1u)
                        IP = targetAddress
                        return true // IP was modified
                    }
                    Operation.OperationOne.JMP -> {
                        IP = read(operand)
                        return true
                    }
                    Operation.OperationOne.JNZ -> {
                        if (!getFlag(EFlags.ZF)) {
                            IP = read(operand)
                            return true
                        }
                    }
                    Operation.OperationOne.JZ -> {
                        if (getFlag(EFlags.ZF)) {
                            IP = read(operand)
                            return true
                        }
                    }
                    Operation.OperationOne.LOOP -> {
                        val ecx = get32(ECX) - 1u
                        set32(ECX, ecx)
                        if (ecx != 0u) {
                            IP = read(operand)
                            return true
                        }
                    }
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
        }
        return false // IP was not modified by jump/call/ret
    }


    fun run(program: List<Instruction>, startAddress: UInt = 0u, maxSteps: Int = 1000) {
        IP = startAddress
        var steps = 0

        // Pre-pass to define all labels, if you had a separate Instruction type for label definitions.
        // Since labels are resolved by `read(LabelOp)` using `symbolTable`,
        // ensure `defineSymbol` is called for all labels before `run` or that
        // LabelOp operands in the program list are resolvable at runtime.

        while (IP < program.size.toUInt() && steps < maxSteps) {
            val instructionAddress = IP
            if (instructionAddress.toInt() >= program.size) {
                println("Program execution stopped: IP (0x${IP.toString(16)}) went out of program bounds (0x${program.size.toUInt().toString(16)}).")
                break
            }
            val instruction = program[instructionAddress.toInt()]

            val ipModifiedByInstruction = execute(instruction)

            if (!ipModifiedByInstruction) {
                IP += 1u // Increment IP only if not a jump/call/ret
            }
            steps++
        }

        if (IP >= program.size.toUInt() && steps < maxSteps) {
            println("Program execution finished normally (IP reached end of program).")
        } else if (steps >= maxSteps) {
            println("Program execution stopped: maximum steps ($maxSteps) reached. IP = 0x${IP.toString(16)}")
        }
    }


    // === Debug helpers ===
    fun get(r: Reg): UInt = when (r) {
        AL,AH,BL,BH,CL,CH,DL,DH -> get8(r).toUInt()
        AX,BX,CX,DX,SI,DI,BP,SP -> get16(r).toUInt()
        else -> get32(r)
    }
    fun set(r: Reg,v:UInt) { write(RegOp(r),v) }

    /**
     * Generates a string representation of the current state of the CPU registers and flags.
     */
    fun printRegisters() {
        println()
        println(" EAX=%08X | AX=%04X | AH=%02X, AL=%02X".format(get32(EAX).toInt(), get16(AX).toInt(), get8(AH).toInt(), get8(AL).toInt()))
        println(" EBX=%08X | BX=%04X | BH=%02X, BL=%02X".format(get32(EBX).toInt(), get16(BX).toInt(), get8(BH).toInt(), get8(BL).toInt()))
        println(" ECX=%08X | CX=%04X | CH=%02X, CL=%02X".format(get32(ECX).toInt(), get16(CX).toInt(), get8(CH).toInt(), get8(CL).toInt()))
        println(" EDX=%08X | DX=%04X | DH=%02X, DL=%02X".format(get32(EDX).toInt(), get16(DX).toInt(), get8(DH).toInt(), get8(DL).toInt()))
        println(" ESI=%08X | SI=%04X".format(get32(ESI).toInt(), get16(SI).toInt()))
        println(" EDI=%08X | DI=%04X".format(get32(EDI).toInt(), get16(DI).toInt()))
        println(" EBP=%08X | BP=%04X".format(get32(EBP).toInt(), get16(BP).toInt()))
        println(" ESP=%08X | SP=%04X".format(get32(ESP).toInt(), get16(SP).toInt()))
        println(" EIP=%08X".format(IP.toInt()))
        println(" CF=%d PF=%d AF=%d ZF=%d SF=%d OF=%d".format(getFlag(EFlags.CF).int, getFlag(EFlags.PF).int, getFlag(EFlags.AF).int, getFlag(EFlags.ZF).int, getFlag(EFlags.SF).int, getFlag(EFlags.OF).int))
        println(" CS=%04X DS=%04X SS=%04X ES=%04X FS=%04X GS=%04X".format(getSeg(SegReg.CS).toInt(), getSeg(SegReg.DS).toInt(), getSeg(SegReg.SS).toInt(), getSeg(SegReg.ES).toInt(), getSeg(SegReg.FS).toInt(), getSeg(SegReg.GS).toInt()))
        println()
    }
    private val Boolean.int get() = if (this) 1 else 0
}

fun main() {
    val mem = Memory(1024f / (1024*1024))
    val cpu = CPU(mem)
    println("Memory size: ${mem.size} bytes")

    // Define some symbols
    cpu.defineSymbol("MY_LABEL", 0x100u)
    cpu.defineSymbol("LOOP_START", 0x200u)

    val program = listOf(
         Instruction.InstructionTwo(Operation.OperationTwo.MOV, RegOp(EAX), MemOp(base = null, disp = 0x10u)), // MOV EAX, [DS:0x10]
         Instruction.InstructionOne(Operation.OperationOne.PUSH, ImmOp(0x1234u)),
         Instruction.InstructionOne(Operation.OperationOne.POP, RegOp(EBX)),

//         Instruction.InstructionOne(Operation.OperationOne.JMP, LabelOp("LOOP_START")),

//         Instruction.InstructionOne(Operation.OperationOne.CALL, LabelOp("MY_SUBROUTINE")),
//         Instruction.InstructionZero(Operation.OperationZero.RET) // Ensure RET is defined
    )
     cpu.run(program, startAddress = 0u, maxSteps = 100)

    cpu.printRegisters()
//    mem.printMemory()
}