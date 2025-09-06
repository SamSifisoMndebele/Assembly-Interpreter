package instruction

import model.Operand.Immediate
import model.Operand.Memory
import model.Operand.Register
import model.Operation
import model.Operation.OperationOne
import model.Operation.OperationTwo
import model.Operation.OperationZero
import model.Reg

/**
 * Represents a single x86 assembly instruction.
 *
 * This interface is the base for all specific instruction types (e.g., those with zero, one, or two operands).
 * It defines common properties like the operation type and the original line number,
 * and mandates methods for encoding the instruction into machine code and generating a string representation.
 *
 * Implementations of this interface will handle the specifics of different x86 instructions,
 * including their opcodes, operands, and addressing modes.
 *
 * The `decode` companion object function provides functionality to parse a sequence of bytes
 * (machine code) back into a list of `Instruction` objects.
 */
@OptIn(ExperimentalUnsignedTypes::class)
sealed interface Instruction {
    /**
     * The operation to be performed.
     */
    val operation: Operation
    /**
     * The line number of the instruction in the source code.
     */
    val line: Int

    /**
     * Encodes the instruction into a sequence of bytes (machine code).
     *
     * @return A UByteArray representing the machine code for this instruction.
     * @throws error if the instruction or its operands are unsupported for encoding.
     */
    fun encode(): UByteArray

    /**
     * Provides a string representation of the instruction, typically for debugging or display.
     *
     * @return A string in the format "line: operation operands".
     */
    abstract override fun toString(): String

    companion object {
        /**
         * Decodes a UByteArray of machine code into a list of Instruction objects.
         * The line number for decoded instructions is set to their byte offset in the input array.
         *
         * @param bytes The UByteArray containing the x86 machine code.
         * @return A list of decoded {@link Instruction} objects.
         * @throws error if an unknown opcode is encountered or an unsupported addressing mode is used.
         */
        fun decode(bytes: UByteArray): List<Instruction> {
            val result = mutableListOf<Instruction>()
            var i = 0
            val r32OpcodeMap = Reg.entries
                .filter { it.name.startsWith("E") }
                .associateBy { it.code.toInt() }

            while (i < bytes.size) {
                val currentOffset = i
                when (val currentOpcode = bytes[i].toInt() and 0xFF) {
                    0x90 -> { // NOP
                        result.add(InstructionZero(OperationZero.NOP, currentOffset))
                        i += 1
                    }
                    0x68 -> { // PUSH imm32
                        val imm = bytes.copyOfRange(i + 1, i + 5).toUInt()
                        result.add(InstructionOne(OperationOne.PUSH, Immediate(imm), currentOffset))
                        i += 5
                    }
                    in 0xB8..0xBF -> { // MOV r32, imm32
                        val regOpcode = currentOpcode - 0xB8
                        val imm = bytes.copyOfRange(i + 1, i + 5).toUInt()
                        val dstReg = r32OpcodeMap[regOpcode]
                            ?: error("Unknown 32-bit destination register opcode: $regOpcode for MOV r32, imm32 instruction at offset $currentOffset")
                        result.add(InstructionTwo(OperationTwo.MOV, Register(dstReg), Immediate(imm), currentOffset))
                        i += 5
                    }
                    0x89 -> { // MOV r/m32, r32  OR MOV r32, r/m32 (check ModR/M)
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        val regOpcode = (modRM shr 3) and 0b111 // Source register for MOV r/m32, r32; Destination for MOV r32, r/m32 (if r/m is memory)
                        val rmOpcode = modRM and 0b111

                        if (mod == 0b11) { // Register-direct addressing: MOV r32, r32
                            val dstReg = r32OpcodeMap[rmOpcode]
                                ?: error("Unknown 32-bit destination register opcode: $rmOpcode in ModR/M ${modRM.toString(16)} at offset $currentOffset")
                            val srcReg = r32OpcodeMap[regOpcode]
                                ?: error("Unknown 32-bit source register opcode: $regOpcode in ModR/M ${modRM.toString(16)} at offset $currentOffset")
                            result.add(InstructionTwo(OperationTwo.MOV, Register(dstReg), Register(srcReg), currentOffset))
                            i += 2
                        } else if (mod == 0b00 && rmOpcode == 0b101) { // Displacement-only addressing: MOV [disp32], r32
                            val srcReg = r32OpcodeMap[regOpcode] ?: error("Unknown 32-bit source register opcode: $regOpcode for MOV [disp32], r32 at offset $currentOffset")
                            val disp = bytes.copyOfRange(i + 2, i + 6).toUInt()
                            result.add(InstructionTwo(OperationTwo.MOV, Memory(null, disp), Register(srcReg), currentOffset))
                            i += 6
                        } else {
                            error("Unsupported MOV 0x89 addressing mode (mod=$mod, r/m=$rmOpcode): ${modRM.toString(16)} at offset $currentOffset")
                        }
                    }
                    0x8B -> { // MOV r32, r/m32 (here, r/m must be memory)
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        val regOpcode = (modRM shr 3) and 0b111 // Destination register
                        val rmOpcode = modRM and 0b111

                        if (mod == 0b00 && rmOpcode == 0b101) { // Displacement-only addressing: MOV r32, [disp32]
                            val dstReg = r32OpcodeMap[regOpcode]
                                ?: error("Unknown 32-bit destination register opcode: $regOpcode for MOV r32, [disp32] at offset $currentOffset")
                            val disp = bytes.copyOfRange(i + 2, i + 6).toUInt()
                            result.add(InstructionTwo(OperationTwo.MOV, Register(dstReg), Memory(null, disp), currentOffset))
                            i += 6
                        } else {
                            error("Unsupported MOV 0x8B addressing mode (mod=$mod, r/m=$rmOpcode): ${modRM.toString(16)} at offset $currentOffset")
                        }
                    }
                    0xC7 -> { // MOV r/m32, imm32
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        val opcodeExtension = (modRM shr 3) and 0b111
                        val rmOpcode = modRM and 0b111

                        if (opcodeExtension == 0b000) { // Check for /0 opcode extension
                            if (mod == 0b00 && rmOpcode == 0b101) { // Displacement-only addressing: MOV [disp32], imm32
                                val disp = bytes.copyOfRange(i + 2, i + 6).toUInt()
                                val imm = bytes.copyOfRange(i + 6, i + 10).toUInt()
                                result.add(InstructionTwo(OperationTwo.MOV, Memory(null, disp), Immediate(imm), currentOffset))
                                i += 10
                            } else {
                                error("Unsupported MOV 0xC7 /0 addressing mode (mod=$mod, r/m=$rmOpcode): ${modRM.toString(16)} at offset $currentOffset")
                            }
                        } else {
                            error("Unsupported MOV 0xC7 opcode extension: $opcodeExtension at offset $currentOffset")
                        }
                    }
                    0x01 -> { // ADD r/m32, r32
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        val srcRegOpcode = (modRM shr 3) and 0b111
                        val dstRmOpcode = modRM and 0b111

                        if (mod == 0b11) { // Register-direct: ADD r32, r32
                            val dstReg = r32OpcodeMap[dstRmOpcode]
                                ?: error("Unknown 32-bit destination register opcode: $dstRmOpcode for ADD r32, r32 at offset $currentOffset")
                            val srcReg = r32OpcodeMap[srcRegOpcode]
                                ?: error("Unknown 32-bit source register opcode: $srcRegOpcode for ADD r32, r32 at offset $currentOffset")
                            result.add(InstructionTwo(OperationTwo.ADD, Register(dstReg), Register(srcReg), currentOffset))
                            i += 2
                        } else {
                            error("Unsupported ADD 0x01 addressing mode (mod=$mod): ${modRM.toString(16)} at offset $currentOffset")
                        }
                    }
                    0x81 -> { // ADD/CMP/SUB r/m32, imm32
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        val opcodeExtension = (modRM shr 3) and 0b111
                        val rmOpcode = modRM and 0b111

                        if (mod == 0b11) { // Register-direct addressing
                            val dstReg = r32OpcodeMap[rmOpcode]
                                ?: error("Unknown 32-bit destination register opcode: $rmOpcode for Op 0x81 at offset $currentOffset")
                            val imm = bytes.copyOfRange(i + 2, i + 6).toUInt()
                            when (opcodeExtension) {
                                0b000 -> result.add(InstructionTwo(OperationTwo.ADD, Register(dstReg), Immediate(imm), currentOffset)) // ADD r32, imm32
                                0b101 -> result.add(InstructionTwo(OperationTwo.SUB, Register(dstReg), Immediate(imm), currentOffset)) // SUB r32, imm32
                                0b111 -> result.add(InstructionTwo(OperationTwo.CMP, Register(dstReg), Immediate(imm), currentOffset)) // CMP r32, imm32
                                else -> error("Unsupported 0x81 opcode extension $opcodeExtension at offset $currentOffset")
                            }
                            i += 6
                        } else {
                            error("Unsupported 0x81 addressing mode (mod=$mod): ${modRM.toString(16)} at offset $currentOffset")
                        }
                    }
                    0x39 -> { // CMP r/m32, r32
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        val srcRegOpcode = (modRM shr 3) and 0b111
                        val dstRmOpcode = modRM and 0b111

                        if (mod == 0b11) { // Register-direct: CMP r32, r32
                            val dstReg = r32OpcodeMap[dstRmOpcode]
                                ?: error("Unknown 32-bit destination register opcode: $dstRmOpcode for CMP r32, r32 at offset $currentOffset")
                            val srcReg = r32OpcodeMap[srcRegOpcode]
                                ?: error("Unknown 32-bit source register opcode: $srcRegOpcode for CMP r32, r32 at offset $currentOffset")
                            // Also handles CMPR as they have the same opcode and encoding
                            result.add(InstructionTwo(OperationTwo.CMP, Register(dstReg), Register(srcReg), currentOffset))
                            i += 2
                        } else {
                            error("Unsupported CMP 0x39 addressing mode (mod=$mod): ${modRM.toString(16)} at offset $currentOffset")
                        }
                    }
                    0x29 -> { // SUB r/m32, r32
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        val srcRegOpcode = (modRM shr 3) and 0b111
                        val dstRmOpcode = modRM and 0b111

                        if (mod == 0b11) { // Register-direct: SUB r32, r32
                            val dstReg = r32OpcodeMap[dstRmOpcode]
                                ?: error("Unknown 32-bit destination register opcode: $dstRmOpcode for SUB r32, r32 at offset $currentOffset")
                            val srcReg = r32OpcodeMap[srcRegOpcode]
                                ?: error("Unknown 32-bit source register opcode: $srcRegOpcode for SUB r32, r32 at offset $currentOffset")
                            result.add(InstructionTwo(OperationTwo.SUB, Register(dstReg), Register(srcReg), currentOffset))
                            i += 2
                        } else {
                            error("Unsupported SUB 0x29 addressing mode (mod=$mod): ${modRM.toString(16)} at offset $currentOffset")
                        }
                    }
                    0x87 -> { // XCHG r/m32, r32 or XCHG r32, r/m32
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        val regOpcode = (modRM shr 3) and 0b111 // This is one register
                        val rmOpcode = modRM and 0b111      // This is the other register or part of memory addressing

                        if (mod == 0b11) { // XCHG r32, r32
                            // In this form, reg field is one register, r/m field is the other.
                            // The encoded instruction 0x87 C_D means XCHG regC, regD (where D is from r/m field)
                            val reg1 = r32OpcodeMap[regOpcode]
                                ?: error("Unknown 32-bit register (reg field): $regOpcode for XCHG r32,r32 at offset $currentOffset")
                            val reg2 = r32OpcodeMap[rmOpcode]
                                ?: error("Unknown 32-bit register (r/m field): $rmOpcode for XCHG r32,r32 at offset $currentOffset")
                            result.add(InstructionTwo(OperationTwo.XCHG, Register(reg2), Register(reg1), currentOffset))
                            i += 2
                        } else if (mod == 0b00 && rmOpcode == 0b101) { // XCHG r32, [disp32] or XCHG [disp32], r32
                            // For 0x87, the register is always in the REG field of ModR/M
                            val reg = r32OpcodeMap[regOpcode]
                                ?: error("Unknown 32-bit register: $regOpcode for XCHG with memory at offset $currentOffset")
                            val disp = bytes.copyOfRange(i + 2, i + 6).toUInt()
                            // The source/dest distinction for XCHG r,m vs m,r is subtle in encoding but clear in our model
                            // We need to infer from the original instruction structure if possible, but here we only have bytes.
                            // The byte sequence is the same for XCHG r32, [disp32] and XCHG [disp32], r32
                            // Let's assume the register from ModR/M's REG field is DST if the other is MEM for now.
                            // This might need adjustment if parsing from a specific textual form to bytes.
                            // For decoding, we can choose one form; the main.kt example has XCHG reg, mem and XCHG mem, reg
                            // XCHG ECX, Memory(Reg.EDX, 10u) -> 87 0A xx xx xx xx (EDX is not part of ModRM reg field)
                            // XCHG Memory(Reg.EDX, 10u), ECX -> 87 0A xx xx xx xx (ECX is ModRM reg field)
                            // The current encode logic for XCHG r32, m32 uses dst.reg for regCode.
                            // The current encode logic for XCHG m32, r32 uses src.reg for regCode.
                            // So, the reg field in ModR/M byte points to the register operand.
                            // The instruction can be interpreted as XCHG Register(reg), Memory(...) OR XCHG Memory(...), Register(reg)
                            // The order is ambiguous from bytes alone without knowing which operand was "first" in the original.
                            // We will decode as XCHG Register(reg), Memory(...) as a convention.
                            result.add(InstructionTwo(OperationTwo.XCHG, Register(reg), Memory(null, disp), currentOffset))
                            i += 6
                        } else {
                            error("Unsupported XCHG 0x87 addressing mode (mod=$mod, r/m=$rmOpcode): ${modRM.toString(16)} at offset $currentOffset")
                        }
                    }
                    else -> error("Unknown opcode: 0x${bytes[i].toString(16)} at offset $currentOffset")
                }
            }
            return result
        }

        private fun UInt.toUBytes(): UByteArray =
            ubyteArrayOf(
                (this and 0xFFu).toUByte(),
                ((this shr 8) and 0xFFu).toUByte(),
                ((this shr 16) and 0xFFu).toUByte(),
                ((this shr 24) and 0xFFu).toUByte()
            )

        private fun UByteArray.toUInt(): UInt = this[0].toUInt() or
                (this[1].toUInt() shl 8) or
                (this[2].toUInt() shl 16) or
                (this[3].toUInt() shl 24)
    }
}