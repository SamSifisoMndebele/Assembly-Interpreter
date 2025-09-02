package model

sealed interface Operation {
    sealed interface OperationZero: Operation {
        data object NOP : OperationZero
        data object PUSHA : OperationZero
        data object POPA : OperationZero
        data object RET: OperationZero
//        data object PUSHAD : OperationZero
//        data object POPAD : OperationZero
//        data object EXIT : OperationZero
//        data object CMPS : OperationZero
//        data object SCAS : OperationZero
//        data object LODS : OperationZero
//        data object STOS : OperationZero
    }
    sealed interface OperationOne: Operation {
        data object PUSH : OperationOne
        data object POP : OperationOne
        data object INC : OperationOne
        data object DEC : OperationOne
        data object CALL : OperationOne
//        data object MUL : OperationOne
//        data object IMUL : OperationOne
//        data object DIV : OperationOne
//        data object IDIV : OperationOne
//        data object NEG : OperationOne
//        data object NOT : OperationOne
//        data object INT : OperationOne
        data object LOOP : OperationOne
        data object JMP : OperationOne
        data object JZ : OperationOne
        data object JNZ : OperationOne
//        data object JE : OperationOne
//        data object JNE : OperationOne
        data object JG : OperationOne // JNLE
//        data object JL : OperationOne // JNGE
//        data object JA : OperationOne // JNBE
//        data object JB : OperationOne // JNAE
//        data object JGE : OperationOne
//        data object JNL : OperationOne
//        data object JLE : OperationOne
//        data object JNG : OperationOne
//        data object JAE : OperationOne
//        data object JNB : OperationOne
//        data object JBE : OperationOne
//        data object JNA : OperationOne
//        data object JO : OperationOne
//        data object JNO : OperationOne
//        data object JS : OperationOne
//        data object JNS : OperationOne
//        data object JP : OperationOne
//        data object JPE : OperationOne
//        data object JNP : OperationOne
//        data object JPO : OperationOne
//        data object JCXZ : OperationOne
//        data object JECXZ : OperationOne
//        data object LOOPZ : OperationOne
//        data object LOOPE : OperationOne
//        data object LOOPNZ : OperationOne
//        data object LOOPNE : OperationOne
    }
    sealed interface OperationTwo: Operation {
        data object MOV : OperationTwo
        data object ADD : OperationTwo
        data object SUB : OperationTwo
        data object XCHG : OperationTwo
        data object CMP : OperationTwo
//        data object MOVS : OperationZero
//        data object MOVZ : OperationZero
//        data object LEA : OperationTwo
//        data object AND : OperationTwo
//        data object OR : OperationTwo
//        data object XOR : OperationTwo
//        data object TEST : OperationTwo
//        data object SHL : OperationTwo
//        data object SAL : OperationTwo
//        data object SHR : OperationTwo
//        data object SAR : OperationTwo
//        data object ROL : OperationTwo
//        data object ROR : OperationTwo
//        data object RCL : OperationTwo
//        data object RCR : OperationTwo
    }
}