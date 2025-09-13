package instruction

import instruction.Instruction.Companion.toUBytes
import model.CpuRegister
import model.Operand
import model.Operation
import model.Operation.OperationOne
import model.Symbol

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
    override fun encode(symbols: Map<String, Symbol>): UByteArray = when (operation) {
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
        OperationOne.PUSH -> when(operand) {
            is Operand.Immediate -> {
                val bytes = operand.value.toUBytes()
                ubyteArrayOf(0x68u, *bytes)
            }
            is Operand.Register -> {
                when (operand.cpuRegister) {
                    CpuRegister.EAX, CpuRegister.ECX, CpuRegister.EDX, CpuRegister.EBX, CpuRegister.ESP, CpuRegister.EBP, CpuRegister.ESI, CpuRegister.EDI -> {
                        ubyteArrayOf((0x50u + operand.cpuRegister.code).toUByte())
                    }
                    CpuRegister.AX, CpuRegister.CX, CpuRegister.DX, CpuRegister.BX, CpuRegister.SP, CpuRegister.BP, CpuRegister.SI, CpuRegister.DI -> {
                        ubyteArrayOf(0x66u, (0x50u + operand.cpuRegister.code).toUByte())
                    }
                    CpuRegister.CS -> ubyteArrayOf(0x0Eu)
                    CpuRegister.SS -> ubyteArrayOf(0x16u)
                    CpuRegister.DS -> ubyteArrayOf(0x1Eu)
                    // Note: ES (0x06u), FS (0x0Fu, 0xA0u), GS (0x0Fu, 0xA8u) are not in CpuRegister.kt yet.
                    // 8-bit registers like AL, AH, etc., cannot be PUSHed directly with this instruction.
                    else -> TODO("PUSH for register ${operand.cpuRegister.name} is not implemented or not supported.")
                }
            }
            is Operand.Identifier -> TODO() // This will likely resolve to an immediate or memory address.
            is Operand.Label -> TODO() // This will resolve to an immediate memory address (offset).
            is Operand.Memory -> TODO() // This would typically be opcode FF /6, requiring ModR/M byte.
        }
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
