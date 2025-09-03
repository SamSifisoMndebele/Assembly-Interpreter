@file:OptIn(ExperimentalUnsignedTypes::class)

package model

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
    abstract fun encode(): UByteArray
    abstract override fun toString(): String


    /**
     * Represents an instruction with no operands.
     * Examples: `nop`, `ret`.
     *
     * @property operation The operation code.
     * @property line The line number in the source file.
     */
    data class InstructionZero(
        val operation: Operation.OperationZero,
        override val line: Int = -1
    ) : Instruction(line) {
        override fun encode(): UByteArray {
            return when (this.operation) {
                Operation.OperationZero.NOP -> ubyteArrayOf(0x90.toUByte())
                else -> error("Unsupported zero-op instruction: ${this.operation}")
            }
        }

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
        val operation: Operation.OperationOne,
        val operand: Operand,
        override val line: Int = -1
    ) : Instruction(line) {
        override fun encode(): UByteArray {
            return when (this.operation) {
                Operation.OperationOne.PUSH -> {
                    val imm = (this.operand as Operand.Immediate).value
                    ubyteArrayOf(0x68.toUByte()) + uIntToBytes(imm)
                }
                else -> error("Unsupported one-op instruction: ${this.operation}")
            }
        }

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
        val operation: Operation.OperationTwo,
        val dst: Operand,
        val src: Operand,
        override val line: Int = -1
    ) : Instruction(line) {
        override fun encode(): UByteArray {
            return when (this.operation) {
                Operation.OperationTwo.MOV -> {
                    val dst = this.dst
                    val src = this.src
                    when (dst) {
                        is Operand.Register -> {
                            when (src) {
                                is Operand.Immediate -> {
                                    val imm = src.value
                                    val opcode = (0xB8 + dst.reg.code.toInt()).toUByte()
                                    ubyteArrayOf(opcode) + Instruction.uIntToBytes(imm)
                                }
                                is Operand.Register -> {
                                    val modRM = (0b11_000_000 or (src.reg.code.toInt() shl 3) or dst.reg.code.toInt()).toUByte()
                                    ubyteArrayOf(0x89.toUByte(), modRM)
                                }
                                is Operand.Memory -> TODO("MOV reg32, mem32 not implemented yet")
                                is Operand.Label -> TODO("MOV reg32, mem32 not implemented yet")
                            }
                        }
                        is Operand.Memory -> error("MOV mem32, ... not implemented yet")
                        else -> error("Unsupported MOV destination operand: $dst")
                    }
                }
                else -> error("Unsupported two-op instruction: ${this.operation}")
            }
        }

        override fun toString(): String = "$line: $operation $dst, $src"
    }

    companion object {
        fun decode(bytes: UByteArray): List<Instruction> {
            val result = mutableListOf<Instruction>()
            var i = 0
            val r32OpcodeMap = Reg.entries
                .filter { it.name.startsWith("E") }
                .associateBy { it.code.toInt() }

            while (i < bytes.size) {
                when (val currentOpcode = bytes[i].toInt() and 0xFF) {
                    0x90 -> {
                        result.add(InstructionZero(Operation.OperationZero.NOP, i))
                        i += 1
                    }
                    0x68 -> {
                        val imm = bytes.copyOfRange(i + 1, i + 5).toUInt() // Uses companion extension
                        result.add(InstructionOne(Operation.OperationOne.PUSH, Operand.Immediate(imm), i))
                        i += 5
                    }
                    in 0xB8..0xBF -> {
                        val regOpcode = currentOpcode - 0xB8
                        val imm = bytes.copyOfRange(i + 1, i + 5).toUInt() // Uses companion extension
                        val dstReg = r32OpcodeMap[regOpcode]
                            ?: error("Unknown 32-bit destination register opcode: $regOpcode for MOV r32, imm32 instruction at offset $i")
                        result.add(InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(dstReg), Operand.Immediate(imm), i))
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
                        result.add(InstructionTwo(Operation.OperationTwo.MOV, Operand.Register(dstReg), Operand.Register(srcReg), i))
                        i += 2
                    }
                    else -> error("Unknown opcode: 0x${bytes[i].toUByte().toString(16)} at offset $i")
                }
            }
            return result
        }

        internal fun uIntToBytes(value: UInt): UByteArray =
            ubyteArrayOf(
                (value and 0xFFu).toUByte(),
                ((value shr 8) and 0xFFu).toUByte(),
                ((value shr 16) and 0xFFu).toUByte(),
                ((value shr 24) and 0xFFu).toUByte()
            )

        internal fun UByteArray.toUInt(): UInt = (this[0].toUInt() and 0xFFu) or
                ((this[1].toUInt() and 0xFFu) shl 8) or
                ((this[2].toUInt() and 0xFFu) shl 16) or
                ((this[3].toUInt() and 0xFFu) shl 24)
    }
}
