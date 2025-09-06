package instruction

import model.Operand
import model.Operation

@OptIn(ExperimentalUnsignedTypes::class)
class InstructionOne(
    override val operation: Operation.OperationOne,
    val operand: Operand,
    override val line: Int
) : Instruction {
    override fun encode(): UByteArray = when (operation) {
        Operation.OperationOne.AND -> TODO()
        Operation.OperationOne.CALL -> TODO()
        Operation.OperationOne.DEC -> TODO()
        Operation.OperationOne.DIV -> TODO()
        Operation.OperationOne.INC -> TODO()
        Operation.OperationOne.JA -> TODO()
        Operation.OperationOne.JAE -> TODO()
        Operation.OperationOne.JB -> TODO()
        Operation.OperationOne.JBE -> TODO()
        Operation.OperationOne.JC -> TODO()
        Operation.OperationOne.JE -> TODO()
        Operation.OperationOne.JG -> TODO()
        Operation.OperationOne.JGE -> TODO()
        Operation.OperationOne.JL -> TODO()
        Operation.OperationOne.JLE -> TODO()
        Operation.OperationOne.JMP -> TODO()
        Operation.OperationOne.JNA -> TODO()
        Operation.OperationOne.JNAE -> TODO()
        Operation.OperationOne.JNB -> TODO()
        Operation.OperationOne.JNBE -> TODO()
        Operation.OperationOne.JNC -> TODO()
        Operation.OperationOne.JNE -> TODO()
        Operation.OperationOne.JNG -> TODO()
        Operation.OperationOne.JNGE -> TODO()
        Operation.OperationOne.JNL -> TODO()
        Operation.OperationOne.JNLE -> TODO()
        Operation.OperationOne.JNO -> TODO()
        Operation.OperationOne.JNP -> TODO()
        Operation.OperationOne.JNS -> TODO()
        Operation.OperationOne.JNZ -> TODO()
        Operation.OperationOne.JO -> TODO()
        Operation.OperationOne.JP -> TODO()
        Operation.OperationOne.JPE -> TODO()
        Operation.OperationOne.JPO -> TODO()
        Operation.OperationOne.JS -> TODO()
        Operation.OperationOne.JZ -> TODO()
        Operation.OperationOne.LOOP -> TODO()
        Operation.OperationOne.LOOPE -> TODO()
        Operation.OperationOne.LOOPNE -> TODO()
        Operation.OperationOne.LOOPNZ -> TODO()
        Operation.OperationOne.LOOPZ -> TODO()
        Operation.OperationOne.MUL -> TODO()
        Operation.OperationOne.NEG -> TODO()
        Operation.OperationOne.NOT -> TODO()
        Operation.OperationOne.OR -> TODO()
        Operation.OperationOne.POP -> TODO()
        Operation.OperationOne.PUSH -> TODO()
        Operation.OperationOne.RCL -> TODO()
        Operation.OperationOne.RCR -> TODO()
        Operation.OperationOne.RET -> TODO()
        Operation.OperationOne.ROL -> TODO()
        Operation.OperationOne.ROR -> TODO()
        Operation.OperationOne.SAL -> TODO()
        Operation.OperationOne.SAR -> TODO()
        Operation.OperationOne.SHL -> TODO()
        Operation.OperationOne.SHR -> TODO()
        Operation.OperationOne.TEST -> TODO()
        Operation.OperationOne.XOR -> TODO()
        Operation.OperationTwo.MOVSX -> TODO()
        Operation.OperationTwo.MOVZX -> TODO()
    }

    override fun toString(): String = "$line: $operation $operand"
}