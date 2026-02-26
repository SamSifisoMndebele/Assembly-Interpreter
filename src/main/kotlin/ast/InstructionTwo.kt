package ast

import assemble.Symbol
import isa.MnemonicTwo

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
@OptIn(ExperimentalUnsignedTypes::class)
class InstructionTwo(
    override val mnemonic: MnemonicTwo,
    val destination: Operand,
    val source: Operand,
    override val line: Int
) : Instruction {
    override fun encode(symbols: Map<String, Symbol>): UByteArray = when (mnemonic) {
        MnemonicTwo.ADD8 -> TODO()
        MnemonicTwo.ADD16 -> TODO()
        MnemonicTwo.ADD32 -> {
            when(destination) {
                is Register -> when(source) {
                    is Register -> {
                        
                        require(destination.cpuRegister.is32Bit && source.cpuRegister.is32Bit) {
                            "Invalid register combination for ADD: ${destination.cpuRegister} and ${source.cpuRegister} at line $line"
                        }
                        val modRM = (0b11_000_000 or (source.cpuRegister.code.toInt() shl 3) or destination.cpuRegister.code.toInt()).toUByte()
                        ubyteArrayOf(0x01.toUByte(), modRM)
                    }
                    is Immediate -> TODO()
                    is Identifier -> TODO()
                    is Label -> TODO()
                    is Memory -> TODO()
                }
                is Memory -> TODO()
                is Identifier -> TODO()
                is Immediate -> TODO()
                is Label -> TODO()
            }
        }
        MnemonicTwo.MOVSX8to16 -> TODO()
        MnemonicTwo.MOVSX8to32 -> TODO()
        MnemonicTwo.MOVSX16to32 -> TODO()
        MnemonicTwo.MOVZX8to16 -> TODO()
        MnemonicTwo.MOVZX8to32 -> TODO()
        MnemonicTwo.MOVZX16to32 -> TODO()
        else -> TODO()
    }

    override fun toString(): String = "$line: $mnemonic $destination, $source"
}