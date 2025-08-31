package ir

data class Instruction(
    val op: Op,
    val dst: Operand? = null,
    val src: Operand? = null,
    val src2: Operand? = null, // (unused for now; reserved for future)
    val line: Int = -1
)