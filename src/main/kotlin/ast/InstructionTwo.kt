package ast

import assemble.Symbol
import isa.MnemonicTwo
import utils.toUBytes

/**
 * Represents a two-operand model.instruction.
 *
 * This class handles the encoding of instructions that operate on two operands,
 * such as MOV, ADD, SUB, etc. It determines the correct opcode and ModR/M byte
 * based on the types of the destination and source operands.
 *
 * @property mnemonic The specific operation to be performed (e.g., MOV, ADD).
 * @property destination The destination operand.
 * @property source The source operand.
 * @property line The line number in the source code where this model.instruction was defined.
 */
class InstructionTwo(
    override val mnemonic: MnemonicTwo,
    val destination: Operand,
    val source: Operand,
    override val line: Int
) : Instruction {
    override fun encode(symbols: Map<String, Symbol>): UByteArray = when (mnemonic) {
        MnemonicTwo.ADD8, MnemonicTwo.ADD16, MnemonicTwo.ADD32 -> encodeRegToRm(destination, source, symbols)
        MnemonicTwo.MOV8, MnemonicTwo.MOV16, MnemonicTwo.MOV32 -> encodeRegToRm(destination, source, symbols)
        MnemonicTwo.XCHG8, MnemonicTwo.XCHG16, MnemonicTwo.XCHG32 -> encodeRegToRm(destination, source, symbols)
        MnemonicTwo.MOVSX8to16, MnemonicTwo.MOVSX8to32, MnemonicTwo.MOVSX16to32 -> encodeRegToRm(destination, source, symbols)
        MnemonicTwo.MOVZX8to16, MnemonicTwo.MOVZX8to32, MnemonicTwo.MOVZX16to32 -> encodeRegToRm(destination, source, symbols)
    }

    @OptIn(ExperimentalUnsignedTypes::class)
    private fun encodeRegToRm(dest: Operand, src: Operand, symbols: Map<String, Symbol>): UByteArray {
        when (dest) {
            is Register -> {
                when (src) {
                    is Register -> {
                        // Register to Register
                        val mod = 0b11
                        val modRM = ((mod shl 6) or (src.cpuRegister.code.toInt() shl 3) or dest.cpuRegister.code.toInt()).toUByte()
                        return ubyteArrayOf(mnemonic.encoding.opcode.first(), modRM)
                    }
                    is Operand.Immediate -> {
                        // Register to Immediate (e.g., MOV EAX, 1234h)
                        // For MOV 32-bit immediate to register, x86 uses opcode 0xB8 + register code
                        if (mnemonic == isa.MnemonicTwo.MOV32) {
                            val opCode = (0xB8u + dest.cpuRegister.code).toUByte()
                            val immBytes = src.value.toUBytes(32, line) // Using your Tools.kt extension
                            return ubyteArrayOf(opCode) + immBytes
                        } else {
                            // Standard ModR/M with /digit extension (e.g., ADD EAX, 1)
                            val mod = 0b11
                            val extension = mnemonic.encoding.modRmExtension ?: 0
                            val modRM = ((mod shl 6) or (extension shl 3) or dest.cpuRegister.code.toInt()).toUByte()
                            val immBytes = src.value.toUBytes(32, line)
                            return ubyteArrayOf(mnemonic.encoding.opcode.first(), modRM) + immBytes
                        }
                    }
                    is Memory -> {
                        // Memory to Register
                        val baseReg = src.base?.cpuRegister?.code?.toInt() ?: 0
                        val modRM = ((dest.cpuRegister.code.toInt() shl 3) or baseReg).toUByte()
                        return ubyteArrayOf(mnemonic.encoding.opcode.first(), modRM)
                    }
                    else -> error("Unsupported source operand for ${mnemonic::class.simpleName}: $src")
                }
            }
            // TODO: Add Memory destination logic (e.g., MOV [EAX], EBX)
            else -> error("Unsupported destination operand combination: $dest, $src")
        }
    }

    override fun toString(): String = "$line: $mnemonic $destination, $source"
}