package ast

import assemble.Symbol
import isa.MnemonicZero

/**
 * Represents an model.instruction with zero operands.
 * These instructions typically perform actions that don't require any data,
 * such as halting the processor or returning from a subroutine.
 *
 * @property mnemonic The specific zero-operand operation to be performed.
 * @property line The line number in the source code where this model.instruction was defined.
 */
class InstructionZero(
    override val mnemonic: MnemonicZero,
    override val line: Int
) : Instruction {
    override fun encode(symbols: Map<String, Symbol>): UByteArray {
        // Zero-operand instructions just return their predefined opcode bytes
        return mnemonic.encoding.opcode.toUByteArray()
    }

    override fun toString(): String = "$line: $mnemonic"
}