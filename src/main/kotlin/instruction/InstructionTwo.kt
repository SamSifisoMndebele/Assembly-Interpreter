package instruction

import instruction.Instruction.Companion.toUBytes
import model.Operand
import model.Operand.Immediate
import model.Operand.Memory
import model.Operand.Register
import model.Operation.OperationTwo

@OptIn(ExperimentalUnsignedTypes::class)
class InstructionTwo(
    override val operation: OperationTwo,
    val destination: Operand,
    val source: Operand,
    override val line: Int
) : Instruction {
    override fun encode(): UByteArray = when (operation) {
        OperationTwo.MOV -> when (destination) {
            is Register -> when (source) {
                is Immediate -> {
                    val opcode = (0xB8 + destination.cpuRegister.code.toInt()).toUByte()
                    ubyteArrayOf(opcode) + source.value.toUBytes()
                }

                is Register -> {
                    val modRM =
                        (0b11_000_000 or (source.cpuRegister.code.toInt() shl 3) or destination.cpuRegister.code.toInt()).toUByte()
                    ubyteArrayOf(0x89.toUByte(), modRM)
                }

                is Memory -> {
                    require(source.disp != null) { "Address not provided for MOV r32, [imm32]" }
                    // MOV r32, [imm32] - Opcode 0x8B /r, ModR/M for [disp32] is 00 reg 101
                    val regCode = destination.cpuRegister.code.toInt()
                    val modRM = (0b101 or (regCode shl 3)).toUByte()
                    ubyteArrayOf(0x8B.toUByte(), modRM) + source.disp.toUByte()
                }

                else -> error("Unsupported MOV destination operand: $source")
            }

            is Memory -> when (source) {
                is Register -> {
                    require(destination.disp != null) { "Address not provided for MOV [imm32], r32" }
                    // MOV [imm32], r32 - Opcode 0x89 /r, ModR/M for [disp32] is 00 reg 101
                    // Note: src register is in the 'reg' field of ModR/M, dst [mem] is in r/m.
                    val regCode = source.cpuRegister.code.toInt()
                    val modRM =
                        (0b101 or (regCode shl 3)).toUByte() // mod=00, r/m=101 (disp32), reg=src.reg
                    ubyteArrayOf(0x89.toUByte(), modRM) + destination.disp.toUByte()
                }

                is Immediate -> {
                    require(destination.disp != null) { "Address not provided for MOV [imm32], imm32" }
                    // MOV [imm32], imm32 - Opcode 0xC7 /0, ModR/M for [disp32] is 00 000 101
                    val modRM =
                        (0b101).toUByte() // mod=00, reg=000 (opcode extension), r/m=101 (disp32)
                    ubyteArrayOf(
                        0xC7.toUByte(),
                        modRM
                    ) + destination.disp.toUByte() + source.value.toUBytes()
                }

                else -> error("Unsupported MOV [mem32], src operand: $source")
            }

            else -> error("Unsupported MOV destination operand: $destination")
        }

        OperationTwo.ADD -> TODO()
        OperationTwo.SUB -> TODO()
        OperationTwo.CMP -> TODO()
        OperationTwo.XCHG -> TODO()
        OperationTwo.AND -> TODO()
        OperationTwo.OR -> TODO()
        OperationTwo.XOR -> TODO()
        OperationTwo.MOVSX -> TODO()
        OperationTwo.MOVZX -> TODO()
    }

    override fun toString(): String = "$line: $operation $destination, $source"
}