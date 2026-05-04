package machine

@OptIn(ExperimentalUnsignedTypes::class)
class VirtualCPU(
    private val memory: Memory,
    startingAddress: Long = 0L,
    stackStartingAddress: Long = memory.bytes - 4L
) {
    // Standard x86 32-bit register mapping
    // 0=EAX, 1=ECX, 2=EDX, 3=EBX, 4=ESP, 5=EBP, 6=ESI, 7=EDI
    private val registers = UIntArray(8)

    // Instruction Pointer
    var eip: Long = startingAddress

    // EFLAGS (Status Register)
    var zf: Boolean = false // Zero Flag
    var sf: Boolean = false // Sign Flag
    var cf: Boolean = false // Carry Flag
    var of: Boolean = false // Overflow Flag

    var isRunning = false

    init {
        // Initialize the stack pointer (ESP = 4)
        registers[4] = stackStartingAddress.toUInt()
    }

    // --- Core Execution Loop ---

    fun run() {
        isRunning = true
        println("CPU Started at EIP: ${eip.toString(16)}h")

        while (isRunning && eip < memory.bytes) {
            step()
        }

        println("CPU Halted.")
    }

    fun step() {
        var opcode = fetchByte().toInt()
        var is16BitOp = false

        // Check for the Operand-Size Override Prefix
        if (opcode == 0x66) {
            is16BitOp = true
            opcode = fetchByte().toInt() // Fetch the REAL opcode
        }

        when (opcode) {
            0x00 -> { // EXIT
                isRunning = false
            }
            0x90 -> { // NOP
            }

            // --- Stack Operations ---
            0x68 -> { // PUSH imm32 / imm16
                if (is16BitOp) {
                    val imm = fetchWord()
                    push16(imm)
                } else {
                    val imm = fetchDWord()
                    push32(imm)
                }
            }
            in 0x50..0x57 -> { // PUSH r16/r32
                val regIndex = opcode - 0x50
                if (is16BitOp) {
                    push16(registers[regIndex].toUShort())
                } else {
                    push32(registers[regIndex])
                }
            }
            in 0x58..0x5F -> { // POP r16/r32
                val regIndex = opcode - 0x58
                if (is16BitOp) {
                    val value16 = pop16()
                    // In x86, popping into AX does not overwrite the top 16 bits of EAX.
                    // We mask out the bottom 16 bits (AX) and insert the new value.
                    registers[regIndex] = (registers[regIndex] and 0xFFFF0000u) or value16.toUInt()
                } else {
                    registers[regIndex] = pop32()
                }
            }

            // --- Data Movement ---
            in 0xB8..0xBF -> { // MOV r32, imm32
                val regIndex = opcode - 0xB8
                val imm = fetchDWord()
                registers[regIndex] = imm
            }
            0x89 -> { // MOV r/m32, r32
                val modRM = fetchByte().toInt()
                val (mod, reg, rm) = decodeModRM(modRM)

                if (mod == 0b11) { // Register to Register
                    registers[rm] = registers[reg]
                } else {
                    // Memory Destination (Simplified: assuming [register] without displacement)
                    val destAddress = registers[rm].toLong()
                    memory.writeDWord(destAddress, registers[reg])
                }
            }
            0x8B -> { // MOV r32, r/m32
                val modRM = fetchByte().toInt()
                val (mod, reg, rm) = decodeModRM(modRM)

                if (mod == 0b11) { // Register to Register
                    registers[reg] = registers[rm]
                } else {
                    // Memory Source
                    val srcAddress = registers[rm].toLong()
                    registers[reg] = memory.readDWord(srcAddress)
                }
            }

            // --- ALU Operations ---
            0x01 -> { // ADD r/m32, r32
                val modRM = fetchByte().toInt()
                val (mod, reg, rm) = decodeModRM(modRM)

                if (mod == 0b11) {
                    val result = registers[rm] + registers[reg]
                    updateFlags(result, registers[rm], registers[reg], isAdd = true)
                    registers[rm] = result
                } else {
                    error("Memory addition not fully implemented.")
                }
            }
            0x29 -> { // SUB r/m32, r32
                val modRM = fetchByte().toInt()
                val (mod, reg, rm) = decodeModRM(modRM)

                if (mod == 0b11) {
                    val result = registers[rm] - registers[reg]
                    updateFlags(result, registers[rm], registers[reg], isAdd = false)
                    registers[rm] = result
                } else {
                    error("Memory subtraction not fully implemented.")
                }
            }
            0x39 -> { // CMP r/m32, r32 (Same as SUB, but don't save result)
                val modRM = fetchByte().toInt()
                val (mod, reg, rm) = decodeModRM(modRM)

                if (mod == 0b11) {
                    val result = registers[rm] - registers[reg]
                    updateFlags(result, registers[rm], registers[reg], isAdd = false)
                }
            }

            // --- Control Flow ---
            0xEB -> { // JMP rel8 (Short Jump)
                val offset = fetchByte().toByte().toLong() // Read as signed 8-bit
                eip += offset
            }
            0x74 -> { // JE / JZ rel8
                val offset = fetchByte().toByte().toLong()
                if (zf) eip += offset
            }
            0x75 -> { // JNE / JNZ rel8
                val offset = fetchByte().toByte().toLong()
                if (!zf) eip += offset
            }

            else -> error("CPU Fault: Unknown Opcode 0x${opcode.toString(16).uppercase()} at EIP ${eip - 1}")
        }
    }

    // --- Helpers ---

    private fun fetchWord(): UShort {
        val value = memory.readWord(eip)
        eip += 2
        return value
    }

    private fun push16(value: UShort) {
        registers[4] = registers[4] - 2u // ESP -= 2
        memory.writeWord(registers[4].toLong(), value)
    }

    private fun pop16(): UShort {
        val value = memory.readWord(registers[4].toLong())
        registers[4] = registers[4] + 2u // ESP += 2
        return value
    }

    private fun fetchByte(): UByte {
        return memory.readByte(eip++)
    }

    private fun fetchDWord(): UInt {
        val value = memory.readDWord(eip)
        eip += 4
        return value
    }

    private fun push32(value: UInt) {
        registers[4] = registers[4] - 4u // ESP -= 4
        memory.writeDWord(registers[4].toLong(), value)
    }

    private fun pop32(): UInt {
        val value = memory.readDWord(registers[4].toLong())
        registers[4] = registers[4] + 4u // ESP += 4
        return value
    }

    /**
     * Decodes the ModR/M byte into its 3 components.
     * @return Triple(Mod, Reg, R/M)
     */
    private fun decodeModRM(modRM: Int): Triple<Int, Int, Int> {
        val mod = (modRM shr 6) and 0b11
        val reg = (modRM shr 3) and 0b111
        val rm = modRM and 0b111
        return Triple(mod, reg, rm)
    }

    private fun updateFlags(result: UInt, op1: UInt, op2: UInt, isAdd: Boolean) {
        zf = (result == 0u)
        sf = (result and 0x80000000u) != 0u

        // Simplified Carry and Overflow for unsigned math
        if (isAdd) {
            cf = result < op1
        } else {
            cf = op1 < op2
        }
    }

    // --- Debugging ---

    fun dumpRegisters() {
        println("--- CPU Registers ---")
        println("EAX: ${registers[0].toString(16).padStart(8, '0').uppercase()}h")
        println("ECX: ${registers[1].toString(16).padStart(8, '0').uppercase()}h")
        println("EDX: ${registers[2].toString(16).padStart(8, '0').uppercase()}h")
        println("EBX: ${registers[3].toString(16).padStart(8, '0').uppercase()}h")
        println("ESP: ${registers[4].toString(16).padStart(8, '0').uppercase()}h")
        println("EBP: ${registers[5].toString(16).padStart(8, '0').uppercase()}h")
        println("ESI: ${registers[6].toString(16).padStart(8, '0').uppercase()}h")
        println("EDI: ${registers[7].toString(16).padStart(8, '0').uppercase()}h")
        println("EIP: ${eip.toString(16).padStart(8, '0').uppercase()}h")
        println("FLAGS: ZF=$zf SF=$sf CF=$cf")
        println("---------------------")
    }
}