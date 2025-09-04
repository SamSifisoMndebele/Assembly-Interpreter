@file:OptIn(ExperimentalUnsignedTypes::class)

package asm

import asm.Instruction.*
import model.Operand
import model.Operand.*
import model.Operation.*
import model.Reg
import kotlin.collections.addAll

/**
 * Represents a single assembly language instruction or a label definition.
 *
 * This sealed class is the base for different types of instructions based on the
 * number of operands they take: zero, one, or two. It also includes a type
 * for label definitions, which are essential for control flow in assembly.
 *
 * Each instruction, regardless of its type, can store the line number from the
 * source file where it was parsed. This is useful for error reporting and debugging.
 *
 * The specific operations and operand types are defined within nested data classes
 * ({@link InstructionZero}, {@link InstructionOne}, {@link InstructionTwo}, {@link LabelDefinition}).
 *
 * @property line The line number in the original assembly source file where this
 *                instruction or label was parsed from. Defaults to -1 if not
 *                applicable or unknown.
 */
sealed class Instruction(
    open val line: Int = -1
) {
    /**
     * Encodes the instruction into a sequence of bytes (machine code).
     *
     * @return A UByteArray representing the machine code for this instruction.
     * @throws error if the instruction or its operands are unsupported for encoding.
     */
    abstract fun encode(): UByteArray

    /**
     * Provides a string representation of the instruction, typically for debugging or display.
     *
     * @return A string in the format "line: operation operands".
     */
    abstract override fun toString(): String

    /**
     * Represents an instruction with no operands.
     * Examples: `nop`, `ret`.
     *
     * @property operation The operation code.
     * @property line The line number in the source file.
     */
    data class InstructionZero(
        val operation: OperationZero,
        override val line: Int = -1
    ) : Instruction(line) {
        /**
         * Encodes the zero-operand instruction into machine code.
         *
         * @return A UByteArray representing the machine code.
         * @throws error if the operation is not supported.
         */
        override fun encode(): UByteArray = ubyteArrayOf(operation.opcode)

        /**
         * Returns a string representation of the zero-operand instruction.
         *
         * @return A string in the format "line: operation".
         */
        override fun toString(): String = "$line: $operation"
    }

    /**
     * Represents an instruction with one operand (typically a destination or a source).
     * Examples: `inc rax`, `push eax`, `jmp label`.
     *
     * @property operation The operation code.
     * @property operand The single operand.
     * @property line The line number in the source file.
     */
    data class InstructionOne(
        val operation: OperationOne,
        val operand: Operand,
        override val line: Int = -1
    ) : Instruction(line) {
        /**
         * Encodes the one-operand instruction into machine code.
         *
         * @return A UByteArray representing the machine code.
         *         For `PUSH imm32`, it's `0x68` followed by the 4-byte immediate value.
         * @throws error if the operation or operand type is not supported.
         *               Currently, only `PUSH Immediate` is supported.
         */
        override fun encode(): UByteArray {
            return when (operation) {
                OperationOne.PUSH -> {
                    val imm = (this.operand as Immediate).value
                    ubyteArrayOf(0x68.toUByte()) + imm.toUBytes()
                }
                else -> error("Unsupported one-op instruction: ${this.operation}")
            }
        }

        /**
         * Returns a string representation of the one-operand instruction.
         *
         * @return A string in the format "line: operation operand".
         */
        override fun toString(): String = "$line: $operation $operand"
    }

    /**
     * Represents an instruction with two operands (typically a destination and a source).
     * Examples: `mov rax, rbx`, `add eax, 5`.
     *
     * @property operation The operation code.
     * @property dst The destination operand.
     * @property src The source operand.
     * @property line The line number in the source file.
     */
    data class InstructionTwo(
        val operation: OperationTwo,
        val dst: Operand,
        val src: Operand,
        override val line: Int = -1
    ) : Instruction(line) {
        /**
         * Encodes the two-operand instruction into machine code.
         *
         * @return A UByteArray representing the machine code.
         *         Supports `MOV reg, imm32` (opcode `0xB8 + reg.code` followed by immediate)
         *         and `MOV reg, reg` (opcodes `0x89` followed by ModR/M byte).
         * @throws error if the operation or operand combination is not supported.
         *               `MOV mem32, ...` and `MOV reg32, mem32/label` are not yet implemented.
         */
        override fun encode(): UByteArray {
            return when (this.operation) {
                OperationTwo.MOV -> {
                    val dst = this.dst
                    val src = this.src
                    when (dst) {
                        is Register -> when (src) {
                            is Immediate -> {
                                val imm = src.value
                                val opcode = (0xB8 + dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(opcode) + imm.toUBytes()
                            }
                            is Register -> {
                                val modRM = (0b11_000_000 or (src.reg.code.toInt() shl 3) or dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(0x89.toUByte(), modRM)
                            }
                            is Memory -> {
                                require(src.disp != null) { "Address not provided for MOV r32, [imm32]" }
                                // MOV r32, [imm32] - Opcode 0x8B /r, ModR/M for [disp32] is 00 reg 101
                                val regCode = dst.reg.code.toInt()
                                val modRM = (0b101 or (regCode shl 3)).toUByte()
                                ubyteArrayOf(0x8B.toUByte(), modRM) + src.disp.toUByte()
                            }
                            else -> error("Unsupported MOV destination operand: $src")
                        }
                        is Memory -> when (src) {
                            is Register -> {
                                require(dst.disp != null) { "Address not provided for MOV [imm32], r32" }
                                // MOV [imm32], r32 - Opcode 0x89 /r, ModR/M for [disp32] is 00 reg 101
                                // Note: src register is in the 'reg' field of ModR/M, dst [mem] is in r/m.
                                val regCode = src.reg.code.toInt()
                                val modRM = (0b101 or (regCode shl 3)).toUByte() // mod=00, r/m=101 (disp32), reg=src.reg
                                ubyteArrayOf(0x89.toUByte(), modRM) + dst.disp.toUByte()
                            }
                            is Immediate -> {
                                require(dst.disp != null) { "Address not provided for MOV [imm32], imm32" }
                                // MOV [imm32], imm32 - Opcode 0xC7 /0, ModR/M for [disp32] is 00 000 101
                                val modRM = (0b101).toUByte() // mod=00, reg=000 (opcode extension), r/m=101 (disp32)
                                ubyteArrayOf(0xC7.toUByte(), modRM) + dst.disp.toUByte() + src.value.toUBytes()
                            }
                            else -> error("Unsupported MOV [mem32], src operand: $src")
                        }
                        else -> error("Unsupported MOV destination operand: $dst")
                    }
                }

                OperationTwo.ADC -> TODO()
                OperationTwo.ADCR -> TODO()
                OperationTwo.ADD -> {
                    val dst = this.dst
                    val src = this.src
                    when (dst) {
                        is Register -> when (src) {
                            is Immediate -> {
                                // ADD r32, imm32 - Opcode 0x81 /0 imm32
                                // ModR/M for register direct: 11 000 reg_code
                                val modRM = (0b11_000_000 or dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(0x81.toUByte(), modRM) + src.value.toUBytes()
                            }
                            is Register -> {
                                // ADD r32, r32 - Opcode 0x01 /r
                                // ModR/M: 11 src_reg_code dst_reg_code
                                val modRM = (0b11_000_000 or (src.reg.code.toInt() shl 3) or dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(0x01.toUByte(), modRM)                            }
                            else -> error("Unsupported ADD destination operand: $src")
                        }
                        else -> error("Unsupported ADD destination operand: $dst")
                    }
                }
                OperationTwo.ADDR -> TODO()
                OperationTwo.AND -> TODO()
                OperationTwo.ANDR -> TODO()
                OperationTwo.CMP -> {
                    val dst = this.dst
                    val src = this.src
                    when (dst) {
                        is Register -> when (src) {
                            is Immediate -> {
                                // CMP r32, imm32 - Opcode 0x81 /7 imm32
                                // ModR/M for register direct: 11 111 reg_code
                                val modRM = (0b11_111_000 or dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(0x81.toUByte(), modRM) + src.value.toUBytes()
                            }
                            is Register -> {
                                // CMP r32, r32 - Opcode 0x39 /r
                                // ModR/M: 11 src_reg_code dst_reg_code
                                val modRM = (0b11_000_000 or (src.reg.code.toInt() shl 3) or dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(0x39.toUByte(), modRM)
                            }
                            else -> error("Unsupported CMP destination operand: $src")
                        }
                        else -> error("Unsupported CMP destination operand: $dst")
                    }
                }
                OperationTwo.CMPR -> {
                    val dst = this.dst
                    val src = this.src
                    when (dst) {
                        is Register -> when (src) {
                            is Register -> {
                                // CMPR r32, r32 (equivalent to CMP r32, r32) - Opcode 0x39 /r
                                // ModR/M: 11 src_reg_code dst_reg_code
                                val modRM = (0b11_000_000 or (src.reg.code.toInt() shl 3) or dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(0x39.toUByte(), modRM)
                            }
                            else -> error("Unsupported CMPR source operand for register destination: $src")
                        }
                        else -> error("Unsupported CMPR destination operand: $dst")
                    }
                }
                OperationTwo.MOVR -> TODO()
                OperationTwo.OR -> TODO()
                OperationTwo.ORR -> TODO()
                OperationTwo.SBB -> TODO()
                OperationTwo.SBBR -> TODO()
                OperationTwo.SUB -> {
                    val dst = this.dst
                    val src = this.src
                    when (dst) {
                        is Register -> when (src) {
                            is Register -> {
                                // SUB r32, r32 - Opcode 0x29 /r
                                // ModR/M: 11 src_reg_code dst_reg_code
                                val modRM = (0b11_000_000 or (src.reg.code.toInt() shl 3) or dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(0x29.toUByte(), modRM)
                            }
                            is Immediate -> {
                                // SUB r32, imm32 - Opcode 0x81 /5 imm32
                                // ModR/M for register direct: 11 101 reg_code
                                val modRM = (0b11_101_000 or dst.reg.code.toInt()).toUByte()
                                ubyteArrayOf(0x81.toUByte(), modRM) + src.value.toUBytes()
                            }
                            else -> error("Unsupported SUB destination operand: $src")
                        }
                        else -> error("Unsupported SUB destination operand: $dst")
                    }
                }
                OperationTwo.SUBR -> TODO()
                OperationTwo.XCHG -> {
                    val dst = this.dst
                    val src = this.src
                    when {
                        dst is Register && src is Register -> {
                            // XCHG r32, r32 - Opcode 0x87 /r
                            // ModR/M: 11 src_reg_code dst_reg_code
                            val modRM = (0b11_000_000 or (src.reg.code.toInt() shl 3) or dst.reg.code.toInt()).toUByte()
                            ubyteArrayOf(0x87.toUByte(), modRM)
                        }
                        dst is Register && src is Memory -> {
                            // XCHG r32, m32 - Opcode 0x87 /r
                            // ModR/M for [disp32]: 00 reg 101
                            require(src.disp != null) { "XCHG r32, [mem] requires a displacement for memory operand" }
                            val regCode = dst.reg.code.toInt()
                            val modRM = (0b101 or (regCode shl 3)).toUByte()
                            ubyteArrayOf(0x87.toUByte(), modRM) + src.disp.toUBytes()
                        }
                        dst is Memory && src is Register -> {
                            // XCHG m32, r32 - Opcode 0x87 /r (same as r32, m32 but ModR/M fields swapped effectively)
                            // ModR/M for [disp32]: 00 reg 101
                            require(dst.disp != null) { "XCHG [mem], r32 requires a displacement for memory operand" }
                            val regCode = src.reg.code.toInt() // src register goes into 'reg' field
                            val modRM = (0b101 or (regCode shl 3)).toUByte() // r/m is [disp32]
                            ubyteArrayOf(0x87.toUByte(), modRM) + dst.disp.toUBytes()
                        }
                        else -> error("Unsupported XCHG operands: $dst, $src")
                    }
                }
                OperationTwo.XOR -> TODO()
                OperationTwo.XORR -> TODO()
            }
        }

        /**
         * Returns a string representation of the two-operand instruction.
         *
         * @return A string in the format "line: operation dst, src".
         */
        override fun toString(): String = "$line: $operation $dst, $src"
    }

    companion object {
        /**
         * Decodes a UByteArray of machine code into a list of Instruction objects.
         * The line number for decoded instructions is set to their byte offset in the input array.
         *
         * @param bytes The UByteArray containing the x86 machine code.
         * @return A list of decoded {@link Instruction} objects.
         * @throws error if an unknown opcode is encountered or an unsupported addressing mode is used.
         *               Currently supports `NOP`, `PUSH imm32`, `MOV r32, imm32`, and `MOV r32, r32`.
         */
        fun decode(bytes: UByteArray): List<Instruction> {
            val result = mutableListOf<Instruction>()
            var i = 0
            val r32OpcodeMap = Reg.entries
                .filter { it.name.startsWith("E") }
                .associateBy { it.code.toInt() }

            while (i < bytes.size) {
                when (val currentOpcode = bytes[i].toInt() and 0xFF) {
                    0x90 -> {
                        result.add(InstructionZero(OperationZero.NOP, i))
                        i += 1
                    }
                    0x68 -> {
                        val imm = bytes.copyOfRange(i + 1, i + 5).toUInt() // Uses companion extension
                        result.add(InstructionOne(OperationOne.PUSH, Immediate(imm), i))
                        i += 5
                    }
                    in 0xB8..0xBF -> {
                        val regOpcode = currentOpcode - 0xB8
                        val imm = bytes.copyOfRange(i + 1, i + 5).toUInt() // Uses companion extension
                        val dstReg = r32OpcodeMap[regOpcode]
                            ?: error("Unknown 32-bit destination register opcode: $regOpcode for MOV r32, imm32 instruction at offset $i")
                        result.add(InstructionTwo(OperationTwo.MOV, Register(dstReg), Immediate(imm), i))
                        i += 5
                    }
                    0x89 -> {
                        val modRM = bytes[i + 1].toInt()
                        val mod = (modRM shr 6) and 0b11
                        if (mod != 0b11) {
                            error("Unsupported MOV addressing mode (mod != 11): ${modRM.toString(16)} at offset $i")
                        }
                        val dstRegOpcode = modRM and 0b111
                        val srcRegOpcode = (modRM shr 3) and 0b111
                        val dstReg = r32OpcodeMap[dstRegOpcode]
                            ?: error("Unknown 32-bit destination register opcode: $dstRegOpcode in ModR/M ${modRM.toString(16)} at offset $i")
                        val srcReg = r32OpcodeMap[srcRegOpcode]
                            ?: error("Unknown 32-bit source register opcode: $srcRegOpcode in ModR/M ${modRM.toString(16)} at offset $i")
                        result.add(InstructionTwo(OperationTwo.MOV, Register(dstReg), Register(srcReg), i))
                        i += 2
                    }
                    else -> error("Unknown opcode: 0x${bytes[i].toString(16)} at offset $i")
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

        private fun UByteArray.toUInt(): UInt = (this[0].toUInt() and 0xFFu) or
                ((this[1].toUInt() and 0xFFu) shl 8) or
                ((this[2].toUInt() and 0xFFu) shl 16) or
                ((this[3].toUInt() and 0xFFu) shl 24)
    }
}


fun main() {
    val instructions = listOf(
        InstructionOne(OperationOne.PUSH, Immediate(0x12abu), 1),
        InstructionTwo(OperationTwo.MOV, Register(Reg.EBX), Immediate(50u), 2),
        InstructionTwo(OperationTwo.MOV, Register(Reg.EBX), Register(Reg.ECX), 3),
        InstructionTwo(OperationTwo.MOV, Register(Reg.ECX), Register(Reg.EBX), 4),
        InstructionTwo(OperationTwo.MOV, Register(Reg.EBX), Register(Reg.ECX), 5),
        InstructionTwo(OperationTwo.XCHG, Register(Reg.ECX), Register(Reg.EBX), 6),
        InstructionTwo(OperationTwo.XCHG, Register(Reg.ECX), Memory(Reg.EDX, 10u), 7),
    )

    val machineCodeParts = mutableListOf<String>()
    val machineCodeFull = mutableListOf<UByte>()

    println("\nInstructions:")
    instructions.forEachIndexed { index, instruction ->
        val encodedBytes = instruction.encode() // Changed here
        val hexString = encodedBytes.joinToString(" ") { "%02X".format(it.toInt()) }
        println("$instruction -> $hexString")
        machineCodeParts.add(hexString)
        machineCodeFull.addAll(encodedBytes.asIterable())
    }

    println("\n Full Raw x86 machine code: " + machineCodeFull.toUByteArray().joinToString(" ") { "%02X".format(it.toInt()) })

    val decoded = Instruction.decode(machineCodeFull.toUByteArray()) // Changed here
    println(" Decoded instructions:")
    decoded.forEach(::println)

}
