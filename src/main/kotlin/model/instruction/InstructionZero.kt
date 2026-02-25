package model.instruction

import model.operation.Operation
import model.Symbol
import model.operation.OperationZero

/**
 * Represents an model.instruction with zero operands.
 * These instructions typically perform actions that don't require any data,
 * such as halting the processor or returning from a subroutine.
 *
 * @property operation The specific zero-operand operation to be performed.
 * @property line The line number in the source code where this model.instruction was defined.
 */
@OptIn(ExperimentalUnsignedTypes::class)
class InstructionZero(
    override val operation: OperationZero,
    override val line: Int
) : Instruction {
    override fun encode(symbols: Map<String, Symbol>): UByteArray = when (operation) {

        else -> TODO()
    }

    override fun toString(): String = "$line: $operation"
}