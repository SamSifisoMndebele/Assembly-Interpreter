package cpu

import model.EFlags
import instruction.Instruction
import instruction.InstructionOne
import instruction.InstructionTwo
import instruction.InstructionZero
import model.Operation
import model.Operand
import model.Operand.*
import model.Operand.Immediate
import model.Reg
import model.Reg.*

/**
 * Simulates a CPU with a set of registers, memory, and an instruction pointer.
 * It can execute a list of [Instruction] objects.
 *
 * @property mem The [Memory] instance that this CPU will operate on.
 * @property labels A map of label names to their corresponding memory addresses (UInt). Defaults to an empty map.
 * @property stackBytes The size of the stack in bytes. Defaults to 8,192 bytes (8KB).
 */
@OptIn(ExperimentalUnsignedTypes::class)
class CPU(private val mem: Memory, private val labels: Map<String, UInt> = emptyMap(), private val stackBytes: Long = 8_192) {
    /**
     * Secondary constructor that allows specifying the stack size in kilobytes.
     * @param stackKb The size of the stack in kilobytes.
     */
    @Suppress("unused")
    constructor(memory: Memory, labels: Map<String, UInt> = emptyMap(), stackKb: Int) : this(memory, labels, stackKb * 1024L)

    private enum class Reg32 { EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP }
    private val regs = UIntArray(8)
    @Suppress("PrivatePropertyName")
    private var EFLAGS: UInt = 0u
    @Suppress("PrivatePropertyName")
    private var EIP: UInt = 0u // Instruction Pointer

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
            else -> error("Unsupported 8-bit register: $reg")
        }
    }
    private fun set8(reg: Reg, value: UByte) {
        val i = reg.reg32
        val raw = regs[i]
        regs[i] = when (reg) {
            AL, BL, CL, DL -> (raw and 0xFFFF_FF00u) or value.toUInt()
            AH, BH, CH, DH -> (raw and 0xFFFF_00FFu) or (value.toUInt() shl 8)
            else -> error("Unsupported 8-bit register: $reg")
        }
    }

    private fun push(value: UInt) {
        val esp = regs[Reg32.ESP.ordinal] - 4u
        if (esp < 0u) error("Stack overflow: ESP would be less than 4 (cannot decrement further).")
        regs[Reg32.ESP.ordinal] = esp
        require(esp < mem.bytes.toUInt()) { // Check lower bound for stack growing down
            "Stack physical write out of bounds: Addr=0x${esp.toString(16)}, MemSize=0x${mem.bytes.toString(16)}"
        }
        mem.writeDWord(esp.toLong(), value)
    }

    private fun pop(): UInt {
        var esp = regs[Reg32.ESP.ordinal]
        require(esp + 3u < mem.bytes.toUInt()) { // Check upper bound for pop
            "Stack physical read out of bounds: Addr=0x${esp.toString(16)}, MemSize=0x${mem.bytes.toString(16)}"
        }
        val value = mem.readDWord(esp.toLong())
        esp += 4u
        regs[Reg32.ESP.ordinal] = esp
        return value
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

    // === Operand access ===
    private fun read(op: Operand): UInt = when (op) {
        is Immediate -> op.value
        is Register -> when (op.reg) {
            AL, AH, BL, BH, CL, CH, DL, DH -> get8(op.reg).toUInt()
            AX, BX, CX, DX, SI, DI, BP, SP -> get16(op.reg).toUInt()
            else -> get32(op.reg)
        }
        is Operand.Memory -> {
            val baseValue = op.base?.let { get32(it) } ?: 0u
            val displacement = op.disp ?: 0u
            val address = baseValue + displacement

            if (address >= mem.bytes.toUInt() || address + 3u >= mem.bytes.toUInt()) { // Assuming DWORD access
                error("Memory read out of bounds: Addr=0x${address.toString(16)}, MemSize=0x${mem.bytes.toString(16)}")
            }
            mem.readDWord(address.toLong())
        }
        is Label -> labels[op.name] ?: error("Undefined label: ${op.name}")
    }

    private fun write(op: Operand, value: UInt) {
        when (op) {
            is Register -> when (op.reg) {
                AL, AH, BL, BH, CL, CH, DL, DH -> set8(op.reg, (value and 0xFFu).toUByte())
                AX, BX, CX, DX, SI, DI, BP, SP -> set16(op.reg, (value and 0xFFFFu).toUShort())
                else -> set32(op.reg, value)
            }
            is Operand.Memory -> {
                val baseValue = op.base?.let { get32(it) } ?: 0u
                val displacement = op.disp ?: 0u
                val address = baseValue + displacement

                if (address >= mem.bytes.toUInt() || address + 3u >= mem.bytes.toUInt()) { // Assuming DWORD access
                    error("Memory write out of bounds: Addr=0x${address.toString(16)}, MemSize=0x${mem.bytes.toString(16)}")
                }
                mem.writeDWord(address.toLong(), value)
            }
            else -> error("Cannot write to $op")
        }
    }

    init {
        regs.fill(0u)

        val esp = mem.bytes.toUInt() // ESP points to the address just after the top of stack
        regs[Reg32.ESP.ordinal] = esp
        val stackLimit = if (mem.bytes >= stackBytes) mem.bytes - stackBytes else 0

        println("Memory size: ${mem.bytes} bytes (${mem.bytes.toString(16).uppercase()}h) ")
        println("Initial ESP (physical top of stack, exclusive): ${esp.toString(16)}")
        require(stackBytes <= mem.bytes) {
            "Stack require $stackBytes bytes, but memory has only ${mem.bytes} bytes."
        }
        println("Stack region: [${stackLimit.toString(16)} to ${(esp - 1u).toString(16)}]")
    }

    /**
     * Executes a single instruction.
     * @return true if IP was modified by a jump/call/ret, false otherwise.
     */
    fun execute(instruction: Instruction): Boolean {
        when(instruction) {
            is InstructionZero -> {
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
                        EIP = pop() // Pop return address from stack into IP
                        return true // IP was modified
                    }

                    Operation.OperationZero.EXIT -> TODO()
                    Operation.OperationZero.POPAD -> TODO()
                    Operation.OperationZero.PUSHAD -> TODO()
                }
            }
            is InstructionOne -> {
                val operand = instruction.operand
                when(instruction.operation) {
                    Operation.OperationOne.DEC -> {
                        val a = read(operand)
                        val res = a - 1u
                        write(operand, res)
                        updateZN(res) 
                        setFlag(EFlags.OF, res == 0x7FFFFFFFu) 
                        setFlag(EFlags.AF, ((a and 0xFu) - 1u) and 0x10u != 0u)
                    }
                    Operation.OperationOne.INC -> {
                        val a = read(operand)
                        val res = a + 1u
                        write(operand, res)
                        updateZN(res) 
                        setFlag(EFlags.OF, res == 0x80000000u) 
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
                        push(EIP + 1u) // Assuming instructions are 1 "unit" (e.g. byte, or entry) long for IP increment
                        EIP = targetAddress
                        return true 
                    }
                    Operation.OperationOne.JMP -> {
                        EIP = read(operand)
                        return true
                    }
                    Operation.OperationOne.JNZ -> {
                        if (!getFlag(EFlags.ZF)) {
                            EIP = read(operand)
                            return true
                        }
                    }
                    Operation.OperationOne.JZ -> {
                        if (getFlag(EFlags.ZF)) {
                            EIP = read(operand)
                            return true
                        }
                    }
                    Operation.OperationOne.LOOP -> {
                        val ecx = get32(ECX) - 1u
                        set32(ECX, ecx)
                        if (ecx != 0u) {
                            EIP = read(operand)
                            return true
                        }
                    }
                    Operation.OperationOne.JG -> { // Jump if Greater (SF=OF and ZF=0)
                        if (getFlag(EFlags.SF) == getFlag(EFlags.OF) && !getFlag(EFlags.ZF)) {
                            EIP = read(operand)
                            return true
                        }
                    }

                    Operation.OperationOne.AND -> TODO()
                    Operation.OperationOne.DIV -> TODO()
                    Operation.OperationOne.JAE -> TODO()
                    Operation.OperationOne.JB -> TODO()
                    Operation.OperationOne.JE -> TODO()
                    Operation.OperationOne.JGE -> TODO()
                    Operation.OperationOne.JL -> TODO()
                    Operation.OperationOne.JLE -> TODO()
                    Operation.OperationOne.JNE -> TODO()
                    Operation.OperationOne.JNO -> TODO()
                    Operation.OperationOne.JNP -> TODO()
                    Operation.OperationOne.JNS -> TODO()
                    Operation.OperationOne.JO -> TODO()
                    Operation.OperationOne.JP -> TODO()
                    Operation.OperationOne.JS -> TODO()
                    Operation.OperationOne.LOOPE -> TODO()
                    Operation.OperationOne.LOOPNE -> TODO()
                    Operation.OperationOne.LOOPNZ -> TODO()
                    Operation.OperationOne.LOOPZ -> TODO()
                    Operation.OperationOne.MUL -> TODO()
                    Operation.OperationOne.NEG -> TODO()
                    Operation.OperationOne.NOT -> TODO()
                    Operation.OperationOne.OR -> TODO()
                    Operation.OperationOne.RCL -> TODO()
                    Operation.OperationOne.RCR -> TODO()
                    Operation.OperationOne.RET -> TODO()
                    Operation.OperationOne.ROL -> TODO()
                    Operation.OperationOne.ROR -> TODO()
                    Operation.OperationOne.SAL -> TODO()
                    Operation.OperationOne.SAR -> TODO()
                    Operation.OperationOne.SHL -> TODO()
                    Operation.OperationOne.SHR -> TODO()
                    Operation.OperationOne.TEST -> TODO()
                    Operation.OperationOne.XOR -> TODO()
                    Operation.OperationTwo.MOVSX -> TODO()
                    Operation.OperationTwo.MOVZX -> TODO()
                    Operation.OperationOne.JA -> TODO()
                    Operation.OperationOne.JBE -> TODO()
                    Operation.OperationOne.JC -> TODO()
                    Operation.OperationOne.JNA -> TODO()
                    Operation.OperationOne.JNAE -> TODO()
                    Operation.OperationOne.JNB -> TODO()
                    Operation.OperationOne.JNBE -> TODO()
                    Operation.OperationOne.JNC -> TODO()
                    Operation.OperationOne.JNG -> TODO()
                    Operation.OperationOne.JNGE -> TODO()
                    Operation.OperationOne.JNL -> TODO()
                    Operation.OperationOne.JNLE -> TODO()
                    Operation.OperationOne.JPE -> TODO()
                    Operation.OperationOne.JPO -> TODO()
                }
            }
            is InstructionTwo -> {
                val dst = instruction.destination
                val src = instruction.source
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
                    Operation.OperationTwo.CMP -> {
                        val a = read(dst)
                        val b = read(src)
                        val res = a - b
                        subFlags(a, b, res)
                    }

                    Operation.OperationTwo.AND -> TODO()
                    Operation.OperationTwo.OR -> TODO()
                    Operation.OperationTwo.XOR -> TODO()
                    Operation.OperationTwo.MOVSX -> TODO()
                    Operation.OperationTwo.MOVZX -> TODO()
                }
            }
        }
        return false
    }

    /**
     * Runs a program consisting of a list of [Instruction]s.
     * @param program The list of instructions to execute.
     * @param startAddress The initial value for the Instruction Pointer (EIP), indicating where to start execution. Defaults to 0.
     * @param maxSteps The maximum number of instructions to execute before stopping. Defaults to 1000.
     */
    fun run(program: List<Instruction>, startAddress: UInt = 0u, maxSteps: Int = 1000) {
        EIP = startAddress
        var steps = 0

        println(program.joinToString("\n") { it.toString() })
        println()

        while (EIP < program.size.toUInt() && steps < maxSteps) {
            println("Executing at EIP=${EIP.toString(16)}")
            if (EIP.toInt() >= program.size) {
                println("Program execution stopped: EIP (0x${EIP.toString(16)}) went out of program bounds (0x${program.size.toUInt().toString(16)}).")
                break
            }
            val instruction = program[EIP.toInt()]

            val ipModifiedByInstruction = execute(instruction)

            if (!ipModifiedByInstruction) {
                EIP += 1u
            }
            steps++
        }

        if (EIP >= program.size.toUInt() && steps < maxSteps) {
            println("Program execution finished normally (IP reached end of program).")
        } else if (steps >= maxSteps) {
            println("Program execution stopped: maximum steps ($maxSteps) reached. IP = 0x${EIP.toString(16)}")
        }
    }

    // === Debug helpers ===
    /**
     * Gets the value of a specified register.
     * Handles 8-bit, 16-bit, and 32-bit registers.
     * @param r The [Reg] enum representing the register to read.
     * @return The value of the register as a [UInt].
     */
    fun get(r: Reg): UInt = when (r) {
        AL,AH,BL,BH,CL,CH,DL,DH -> get8(r).toUInt()
        AX,BX,CX,DX,SI,DI,BP,SP -> get16(r).toUInt()
        else -> get32(r)
    }
    /**
     * Sets the value of a specified register.
     * @param r The [Reg] enum representing the register to write to.
     * @param v The [UInt] value to write to the register.
     */
    fun set(r: Reg,v:UInt) { write(Register(r),v) }

    companion object {
        const val RESET = "[0m"
        const val RED = "[31m"
        const val GREEN = "[32m"
        const val YELLOW = "[33m"
        const val BLUE = "[34m"
    }

    /**
     * Generates a string representation of the current state of the CPU registers and flags.
     */
    fun printRegisters() {
        println("${BLUE}Registers Dump:${RESET}")
        println(" ${BLUE}EAX=${RESET}${GREEN}%08X${RESET} | ${BLUE}AX=${RESET}${GREEN}%04X${RESET} | ${BLUE}AH=${RESET}${GREEN}%02X${RESET}, ${BLUE}AL=${RESET}${GREEN}%02X${RESET}".format(get32(EAX).toInt(), get16(AX).toInt(), get8(AH).toInt(), get8(AL).toInt()))
        println(" ${BLUE}EBX=${RESET}${GREEN}%08X${RESET} | ${BLUE}BX=${RESET}${GREEN}%04X${RESET} | ${BLUE}BH=${RESET}${GREEN}%02X${RESET}, ${BLUE}BL=${RESET}${GREEN}%02X${RESET}".format(get32(EBX).toInt(), get16(BX).toInt(), get8(BH).toInt(), get8(BL).toInt()))
        println(" ${BLUE}ECX=${RESET}${GREEN}%08X${RESET} | ${BLUE}CX=${RESET}${GREEN}%04X${RESET} | ${BLUE}CH=${RESET}${GREEN}%02X${RESET}, ${BLUE}CL=${RESET}${GREEN}%02X${RESET}".format(get32(ECX).toInt(), get16(CX).toInt(), get8(CH).toInt(), get8(CL).toInt()))
        println(" ${BLUE}EDX=${RESET}${GREEN}%08X${RESET} | ${BLUE}DX=${RESET}${GREEN}%04X${RESET} | ${BLUE}DH=${RESET}${GREEN}%02X${RESET}, ${BLUE}DL=${RESET}${GREEN}%02X${RESET}".format(get32(EDX).toInt(), get16(DX).toInt(), get8(DH).toInt(), get8(DL).toInt()))
        println(" ${BLUE}ESI=${RESET}${GREEN}%08X${RESET} | ${BLUE}SI=${RESET}${GREEN}%04X${RESET}".format(get32(ESI).toInt(), get16(SI).toInt()))
        println(" ${BLUE}EDI=${RESET}${GREEN}%08X${RESET} | ${BLUE}DI=${RESET}${GREEN}%04X${RESET}".format(get32(EDI).toInt(), get16(DI).toInt()))
        println(" ${BLUE}EBP=${RESET}${GREEN}%08X${RESET} | ${BLUE}BP=${RESET}${GREEN}%04X${RESET}".format(get32(EBP).toInt(), get16(BP).toInt()))
        println(" ${BLUE}ESP=${RESET}${GREEN}%08X${RESET} | ${BLUE}SP=${RESET}${GREEN}%04X${RESET}".format(get32(ESP).toInt(), get16(SP).toInt()))
        println(" ${BLUE}EIP=${RESET}${GREEN}%08X${RESET}".format(EIP.toInt()))
        println(" ${BLUE}EFL=${RESET}${GREEN}%08X${RESET} | ${YELLOW}CF=${RESET}${GREEN}%d${RESET} ${YELLOW}PF=${RESET}${GREEN}%d${RESET} ${YELLOW}AF=${RESET}${GREEN}%d${RESET} ${YELLOW}ZF=${RESET}${GREEN}%d${RESET} ${YELLOW}SF=${RESET}${GREEN}%d${RESET} ${YELLOW}OF=${RESET}${GREEN}%d${RESET}".format(EFLAGS.toInt(), getFlag(EFlags.CF).int, getFlag(EFlags.PF).int, getFlag(EFlags.AF).int, getFlag(EFlags.ZF).int, getFlag(EFlags.SF).int, getFlag(EFlags.OF).int))
        println()
    }
    private val Boolean.int get() = if (this) 1 else 0
}

fun main() {
    val mem = Memory(1024)
    val cpu = CPU(mem, stackBytes = 256)
    println("--- CPU State After Initialization ---")
    cpu.printRegisters()
    println("------------------------------------")

    val program = listOf(
        InstructionTwo(Operation.OperationTwo.MOV, Register(EAX), Immediate(0xAu), 1), // MOV EAX, 0xA
        InstructionTwo(Operation.OperationTwo.MOV, Register(EBX), Immediate(0xBu), 2), // MOV EBX, 0xB
        InstructionOne(Operation.OperationOne.PUSH, Register(EBX), 3),              // PUSH EBX
        InstructionOne(Operation.OperationOne.POP, Register(ECX), 4),               // POP ECX (should be 0xB)
        InstructionOne(Operation.OperationOne.POP, Register(EDX), 5)                // POP EDX (should be 0xA)
    )
    
    println("--- Running Program ---")
    cpu.run(program, startAddress = 0u, maxSteps = 100)
    println("---------------------")

    println("--- CPU State After Program Execution ---")
    cpu.printRegisters()
    println("---------------------------------------")

    // Test memory operation
    val dataAddress = 0x100u // Example physical address
    // Ensure this address is valid and writable
    if (dataAddress + 3u < mem.bytes.toUInt()) {
        println("--- Testing Memory Write/Read ---")
        // MOV [0x100], EAX (where EAX is 0xA after first POP)
        cpu.execute(InstructionTwo(Operation.OperationTwo.MOV, Memory(null, dataAddress), Register(EDX), 1))
        // MOV EDI, [0x100]
        cpu.execute(InstructionTwo(Operation.OperationTwo.MOV, Register(EDI), Memory(null, dataAddress), 2))
        cpu.printRegisters()
        println("Value at mem[0x100]: 0x${mem.readDWord(dataAddress.toLong()).toString(16)}")
        println("---------------------------------")
    } else {
        println("Skipping memory write/read test as address 0x100 is out of bounds for the current memory size.")
    }
}
