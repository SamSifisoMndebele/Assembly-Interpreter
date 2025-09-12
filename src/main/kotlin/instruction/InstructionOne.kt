package instruction

import model.Operand
import model.Operation
import model.Operation.OperationOne

/**
 * Represents an instruction with one operand.
 * @property operation The operation to be performed.
 * @property operand The operand of the instruction.
 * @property line The line number of the instruction in the source code.
 */
@OptIn(ExperimentalUnsignedTypes::class)
class InstructionOne(
    override val operation: OperationOne,
    val operand: Operand,
    override val line: Int
) : Instruction {
    override fun encode(): UByteArray = when (operation) {
        OperationOne.AND -> TODO()
        OperationOne.CALL -> TODO()
        OperationOne.DEC -> TODO()
        OperationOne.DIV -> TODO()
        OperationOne.INC -> TODO()
        OperationOne.JA -> TODO()
        OperationOne.JAE -> TODO()
        OperationOne.JB -> TODO()
        OperationOne.JBE -> TODO()
        OperationOne.JC -> TODO()
        OperationOne.JE -> TODO()
        OperationOne.JG -> TODO()
        OperationOne.JGE -> TODO()
        OperationOne.JL -> TODO()
        OperationOne.JLE -> TODO()
        OperationOne.JMP -> TODO()
        OperationOne.JNA -> TODO()
        OperationOne.JNAE -> TODO()
        OperationOne.JNB -> TODO()
        OperationOne.JNBE -> TODO()
        OperationOne.JNC -> TODO()
        OperationOne.JNE -> TODO()
        OperationOne.JNG -> TODO()
        OperationOne.JNGE -> TODO()
        OperationOne.JNL -> TODO()
        OperationOne.JNLE -> TODO()
        OperationOne.JNO -> TODO()
        OperationOne.JNP -> TODO()
        OperationOne.JNS -> TODO()
        OperationOne.JNZ -> TODO()
        OperationOne.JO -> TODO()
        OperationOne.JP -> TODO()
        OperationOne.JPE -> TODO()
        OperationOne.JPO -> TODO()
        OperationOne.JS -> TODO()
        OperationOne.JZ -> TODO()
        OperationOne.LOOP -> TODO()
        OperationOne.LOOPE -> TODO()
        OperationOne.LOOPNE -> TODO()
        OperationOne.LOOPNZ -> TODO()
        OperationOne.LOOPZ -> TODO()
        OperationOne.MUL -> TODO()
        OperationOne.NEG -> TODO()
        OperationOne.NOT -> TODO()
        OperationOne.OR -> TODO()
        OperationOne.POP -> TODO()
        OperationOne.PUSH -> TODO()
        OperationOne.RCL -> TODO()
        OperationOne.RCR -> TODO()
        OperationOne.RET -> TODO()
        OperationOne.ROL -> TODO()
        OperationOne.ROR -> TODO()
        OperationOne.SAL -> TODO()
        OperationOne.SAR -> TODO()
        OperationOne.SHL -> TODO()
        OperationOne.SHR -> TODO()
        OperationOne.TEST -> TODO()
        OperationOne.XOR -> TODO()
    }

    override fun toString(): String = "$line: $operation $operand"
}