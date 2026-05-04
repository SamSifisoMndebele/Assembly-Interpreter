package ast

import assemble.Symbol
import isa.CpuRegister
import isa.MnemonicOne
import utils.toUBytes

/**
 * Represents an model.instruction with one operand.
 * @property mnemonic The operation to be performed.
 * @property operand The operand of the model.instruction.
 * @property line The line number of the model.instruction in the source code.
 */
@OptIn(ExperimentalUnsignedTypes::class)
class InstructionOne(
    override val mnemonic: MnemonicOne,
    val operand: Operand,
    override val line: Int
) : Instruction {
    override fun encode(symbols: Map<String, Symbol>): UByteArray {
        return when (mnemonic) {
            MnemonicOne.PUSH -> when(operand) {
                is Operand.Immediate -> {
                    // PUSH imm16 → 0x66 0x68 + imm16
                    // PUSH imm32 → 0x68 + imm32 (little-endian)
                    val bytes = operand.value.toUBytes(32, line)
                    ubyteArrayOf(0x68u, *bytes)
                }
                is Register -> {
                    when {
                        operand.cpuRegister.is16Bit -> {
                            ubyteArrayOf(0x66u, (0x50u + operand.cpuRegister.code).toUByte()) // PUSH r16
                        }
                        operand.cpuRegister.is32Bit -> {
                            ubyteArrayOf((0x50u + operand.cpuRegister.code).toUByte()) // PUSH r32
                        }
                        // Segment registers
                        operand.cpuRegister == CpuRegister.CS -> ubyteArrayOf(0x0Eu)
                        operand.cpuRegister == CpuRegister.SS -> ubyteArrayOf(0x16u)
                        operand.cpuRegister == CpuRegister.DS -> ubyteArrayOf(0x1Eu)
                        operand.cpuRegister == CpuRegister.ES -> ubyteArrayOf(0x06u)
    //                    CpuRegister.FS -> ubyteArrayOf(0x0Fu, 0xA0u)
    //                    CpuRegister.GS -> ubyteArrayOf(0x0Fu, 0xA8u)

                        // 8-bit registers are not directly PUSH able
                        else -> error("PUSH for register ${operand.cpuRegister.name} is not supported.")
                    }
                }
                is Identifier -> {
                    // Lookup the identifier in the symbol table
                    val symbol = symbols[operand.name] ?: error("Undefined identifier '${operand.name}' at line $line")

                    // Use the symbol address as a 32-bit displacement
                    val disp = symbol.address.toInt()

                    // Displacement bytes (32-bit little-endian)
                    val dispBytes = ubyteArrayOf(
                        (disp and 0xFF).toUByte(),
                        ((disp shr 8) and 0xFF).toUByte(),
                        ((disp shr 16) and 0xFF).toUByte(),
                        ((disp shr 24) and 0xFF).toUByte()
                    )

                    ubyteArrayOf(0xFFu, 0x25u, *dispBytes)
                }
                is Label -> error("Label cannot be used as an operand for PUSH")
                is Memory -> {
                    // PUSH r/m16/32 → opcode 0xFF /6
                    // Build ModR/M byte directly:
                    // mod = 00 (no displacement) / 01 (8-bit disp) / 10 (32-bit disp)
                    // reg = 6 (PUSH)
                    // rm  = register code for memory base
                    val disp = operand.displacement ?: 0
                    if (operand.base == null && operand.index != null) {
                        val indexCode = operand.index.cpuRegister.code
                        val modrm: UByte = 0x00u.toUByte() or (6u shl 3).toUByte() or 0x04u // rm=100 → SIB present
                        val sib: UByte = ((operand.scale.toInt() and 0x3) shl 6).toUByte() or
                                ((indexCode.toUInt() and 0x7u) shl 3).toUByte() or
                                0x05u // base=101 → no base, disp32 follows
                        val dispBytes = ubyteArrayOf(
                            (disp and 0xFF).toUByte(),
                            ((disp shr 8) and 0xFF).toUByte(),
                            ((disp shr 16) and 0xFF).toUByte(),
                            ((disp shr 24) and 0xFF).toUByte()
                        )
                        return ubyteArrayOf(0xFFu, modrm, sib, *dispBytes)
                    }


                    val baseRegCode = operand.base?.cpuRegister?.code ?: 0u      // e.g., EAX = 0
                    val mod: UByte = when {
                        disp == 0L && baseRegCode != 5.toUByte() -> 0x00u        // mod 00
                        disp in -128..127 -> 0x40u                              // mod 01, 8-bit displacement
                        else -> 0x80u                                           // mod 10, 32-bit displacement
                    }

                    val rm = baseRegCode and 0x07u                              // last 3 bits for rm
                    val modrm = mod or (6u shl 3).toUByte() or rm               // /6 = reg=6

                    val dispBytes = when (mod.toUInt()) {
                        0x40u -> ubyteArrayOf((disp and 0xFF).toUByte())        // 8-bit displacement
                        0x80u -> ubyteArrayOf(
                            (disp and 0xFF).toUByte(),
                            ((disp shr 8) and 0xFF).toUByte(),
                            ((disp shr 16) and 0xFF).toUByte(),
                            ((disp shr 24) and 0xFF).toUByte()
                        ) // 32-bit displacement
                        else -> ubyteArrayOf()
                    }
                    if (rm == 4u.toUByte()) {
                        // SIB: scale=0, index=ESP(4), base=ESP(4)
                        ubyteArrayOf(0xFFu, modrm, 0x24u, *dispBytes)
                    } else {
                        ubyteArrayOf(0xFFu, modrm, *dispBytes)
                    }
                }
            }
            MnemonicOne.POP -> when(operand) {
                is Register -> {
                    when {
                        operand.cpuRegister.is16Bit -> {
                            ubyteArrayOf(0x66u, (0x58u + operand.cpuRegister.code).toUByte()) // POP r16
                        }
                        operand.cpuRegister.is32Bit -> {
                            ubyteArrayOf((0x58u + operand.cpuRegister.code).toUByte()) // POP r32
                        }
                        // Segment registers
                        operand.cpuRegister == CpuRegister.DS -> ubyteArrayOf(0x1Fu)
                        operand.cpuRegister == CpuRegister.ES -> ubyteArrayOf(0x07u)
                        operand.cpuRegister == CpuRegister.SS -> ubyteArrayOf(0x17u)
                        else -> error("POP for register ${operand.cpuRegister.name} is not supported.")
                    }
                }
                is Memory -> {
                    // POP r/m16/32 is opcode 0x8F /0
                    val baseRegCode = operand.base?.cpuRegister?.code ?: 0u
                    val mod: UByte = if (operand.displacement == null) 0x00u else 0x80u // Simplified for now
                    val rm = baseRegCode and 0x07u
                    val modrm = mod or (0u shl 3).toUByte() or rm // /0 = reg=0
                    ubyteArrayOf(0x8Fu, modrm)
                }
                else -> error("Invalid operand for POP: $operand at line $line")
            }
        }
    }

    override fun toString(): String = "$line: $mnemonic $operand"
}