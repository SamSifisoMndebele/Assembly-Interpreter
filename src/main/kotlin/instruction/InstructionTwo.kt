package instruction

import instruction.Instruction.Companion.toUBytes
import model.CpuRegister
import model.Operand
import model.Operand.Immediate
import model.Operand.Memory
import model.Operand.Register
import model.Operation.OperationTwo
import model.Symbol

/**
 * Represents a two-operand instruction.
 *
 * This class handles the encoding of instructions that operate on two operands,
 * such as MOV, ADD, SUB, etc. It determines the correct opcode and ModR/M byte
 * based on the types of the destination and source operands.
 *
 * @property operation The specific operation to be performed (e.g., MOV, ADD).
 * @property destination The destination operand.
 * @property source The source operand.
 * @property line The line number in the source code where this instruction was defined.
 */
@OptIn(ExperimentalUnsignedTypes::class)
class InstructionTwo(
    override val operation: OperationTwo,
    val destination: Operand,
    val source: Operand,
    override val line: Int
) : Instruction {
    override fun encode(symbols: Map<String, Symbol>): UByteArray = when (operation) {
        OperationTwo.MOV -> when (destination) {
            is Register -> when (source) {
                is Immediate -> {
                    val opcode = (0xB8 + destination.cpuRegister.code.toInt()).toUByte()
                    ubyteArrayOf(opcode) + source.value.toUBytes()
                }
                is Register -> {
                    val modRM = (0b11_000_000 or (source.cpuRegister.code.toInt() shl 3) or destination.cpuRegister.code.toInt()).toUByte()
                    ubyteArrayOf(0x89.toUByte(), modRM)
                }
                is Memory -> {
                    val destReg = destination.cpuRegister.code.toInt()
                    val bytes = mutableListOf<UByte>()

                    // Determine if SIB is needed
                    val needsSIB = source.index != null || source.base?.cpuRegister == CpuRegister.ESP

                    // Determine mod field (0,1,2) based on displacement
                    val disp = source.disp ?: 0L
                    val mod = when {
                        disp == 0L && source.base?.cpuRegister != CpuRegister.EBP -> 0b00
                        disp in Byte.MIN_VALUE..Byte.MAX_VALUE -> 0b01 // 8-bit displacement
                        else -> 0b10 // 32-bit displacement
                    }

                    // Determine R/M field
                    val rm = if (needsSIB) 0b100 else source.base?.cpuRegister?.code?.toInt() ?: 0b101

                    val modRM = ((mod shl 6) or (destReg shl 3) or rm).toUByte()
                    bytes += 0x8B.toUByte() // opcode
                    bytes += modRM

                    // Add SIB if needed
                    if (needsSIB) {
                        val scaleBits = when (source.scale) {
                            1u -> 0b00
                            2u -> 0b01
                            4u -> 0b10
                            8u -> 0b11
                            else -> error("Invalid scale: ${source.scale}")
                        }

                        val indexBits = source.index?.cpuRegister?.code?.toInt() ?: 0b100 // 100 = no index
                        val baseBits = source.base?.cpuRegister?.code?.toInt() ?: 0b101    // 101 = no base / disp32
                        val sib = ((scaleBits shl 6) or (indexBits shl 3) or baseBits).toUByte()
                        bytes += sib
                    }

                    // Add displacement bytes
                    if (mod == 0b01) {
                        bytes += disp.toByte().toUByte() // 8-bit displacement
                    } else if ((mod == 0b00 && source.base == null) || mod == 0b10) {
                        bytes += disp.toUInt().toUBytes() // 32-bit displacement
                    }

                    bytes.toUByteArray()
                }

                is Operand.Identifier -> {
                    val address = symbols[source.name] ?: error("Symbol '${source.name}' not found at line $line")
                    // MOV r32, [imm32] - Opcode 0x8B /r, ModR/M for [disp32] is 00 reg 101
                    val regCode = destination.cpuRegister.code.toInt()
                    val modRM = (0b101 or (regCode shl 3)).toUByte()
                    ubyteArrayOf(0x8B.toUByte(), modRM) + address.address.toUInt().toUBytes()
                }
                else -> error("Unsupported MOV source operand: $source")
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

                else -> error("Unsupported MOV [mem32], source operand: $source")
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