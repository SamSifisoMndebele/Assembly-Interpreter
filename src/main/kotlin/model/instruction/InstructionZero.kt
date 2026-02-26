package model.instruction

import model.Symbol
import model.mnemonic.MnemonicZero

/**
 * Represents an model.instruction with zero operands.
 * These instructions typically perform actions that don't require any data,
 * such as halting the processor or returning from a subroutine.
 *
 * @property mnemonic The specific zero-operand operation to be performed.
 * @property line The line number in the source code where this model.instruction was defined.
 */
@OptIn(ExperimentalUnsignedTypes::class)
class InstructionZero(
    override val mnemonic: MnemonicZero,
    override val line: Int
) : Instruction {
    override fun encode(symbols: Map<String, Symbol>): UByteArray = when (mnemonic) {

        else -> TODO()
    }

    override fun toString(): String = "$line: $mnemonic"
}