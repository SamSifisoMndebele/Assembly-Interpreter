@file:OptIn(ExperimentalUnsignedTypes::class)

package asm

import asm.Instruction2.*
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
sealed class Instruction2(
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
    ) : Instruction2(line) {
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
    ) : Instruction2(line) {
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
    ) : Instruction2(line) {
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
                OperationTwo.AND -> TODO()
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
                OperationTwo.OR -> TODO()
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
                OperationTwo.MOVSX -> TODO()
                OperationTwo.MOVZX -> TODO()
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
         */
        fun decode(bytes: UByteArray): List<Instruction2> {
            val result = mutableListOf<Instruction2>()
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
        InstructionTwo(OperationTwo.XCHG, Register(Reg.ECX), Register(Reg.EBX), 6), // 87 D9
        InstructionTwo(OperationTwo.XCHG, Register(Reg.ECX), Memory(null, 10u), 7), // ECX is reg field (001), 87 0D 0A000000
        InstructionTwo(OperationTwo.MOV, Memory(null, 0x100u), Register(Reg.EAX), 8), // EAX is reg field (000), 89 05 00010000
        InstructionTwo(OperationTwo.MOV, Register(Reg.EDX), Memory(null, 0x200u), 9), // EDX is reg field (010), 8B 15 00020000
        InstructionTwo(OperationTwo.MOV, Memory(null, 0x300u), Immediate(0x1234u), 10), // C7 05 00030000 34120000
        InstructionTwo(OperationTwo.ADD, Register(Reg.EAX), Immediate(100u), 11), // 81 C0 64000000
        InstructionTwo(OperationTwo.ADD, Register(Reg.EAX), Register(Reg.EBX), 12), // 01 D8
        InstructionTwo(OperationTwo.CMP, Register(Reg.ECX), Immediate(200u), 13), // 81 F9 C8000000
        InstructionTwo(OperationTwo.CMP, Register(Reg.ECX), Register(Reg.EDX), 14), // 39 D1
        InstructionTwo(OperationTwo.SUB, Register(Reg.ESI), Register(Reg.EDI), 15), // 29 FE
        InstructionTwo(OperationTwo.SUB, Register(Reg.EDI), Immediate(5u), 16), // 81 EF 05000000
        InstructionTwo(OperationTwo.XCHG, Memory(null, 0x400u), Register(Reg.ESP),17) // ESP is reg field (100), 87 25 00040000
    )

    val machineCodeParts = mutableListOf<String>()
    val machineCodeFull = mutableListOf<UByte>()

    println("\nInstructions:")
    instructions.forEachIndexed { index, instruction ->
        val encodedBytes = instruction.encode() 
        val hexString = encodedBytes.joinToString(" ") { "%02X".format(it.toInt()) }
        println("$instruction -> $hexString")
        machineCodeParts.add(hexString)
        machineCodeFull.addAll(encodedBytes.asIterable())
    }

    println("\n Full Raw x86 machine code: " + machineCodeFull.toUByteArray().joinToString(" ") { "%02X".format(it.toInt()) })

    val decoded = Instruction2.decode(machineCodeFull.toUByteArray())
    println(" Decoded instructions:")
    decoded.forEach(::println)

}

