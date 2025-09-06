package instruction

import model.Operand
import model.Operation

@OptIn(ExperimentalUnsignedTypes::class)
class InstructionTwo(
    override val operation: Operation.OperationTwo,
    val destination: Operand,
    val source: Operand,
    override val line: Int
) : Instruction {
    override fun encode(): UByteArray = when(operation) {
        Operation.OperationTwo.MOV -> TODO()
        Operation.OperationTwo.ADD -> TODO()
        Operation.OperationTwo.SUB -> TODO()
        Operation.OperationTwo.CMP -> TODO()
        Operation.OperationTwo.XCHG -> TODO()
        Operation.OperationTwo.AND -> TODO()
        Operation.OperationTwo.OR -> TODO()
        Operation.OperationTwo.XOR -> TODO()
    }

    override fun toString(): String = "$line: $operation $destination, $source"
}