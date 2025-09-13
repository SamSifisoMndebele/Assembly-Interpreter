package instruction

import model.Operation
import model.Symbol

/**
 * Represents an instruction with zero operands.
 * These instructions typically perform actions that don't require any data,
 * such as halting the processor or returning from a subroutine.
 *
 * @property operation The specific zero-operand operation to be performed.
 * @property line The line number in the source code where this instruction was defined.
 */
@OptIn(ExperimentalUnsignedTypes::class)
class InstructionZero(
    override val operation: Operation.OperationZero,
    override val line: Int
) : Instruction {
    override fun encode(symbols: Map<String, Symbol>): UByteArray = when (operation) {
        Operation.OperationZero.EXIT -> TODO()
        Operation.OperationZero.NOP -> TODO()
        Operation.OperationZero.POPA -> TODO()
        Operation.OperationZero.POPAD -> TODO()
        Operation.OperationZero.PUSHA -> TODO()
        Operation.OperationZero.PUSHAD -> TODO()
        Operation.OperationZero.RET -> TODO()
    }

    override fun toString(): String = "$line: $operation"
}