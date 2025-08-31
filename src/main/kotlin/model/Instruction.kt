package model

data class Instruction(
    val op: Op,
    val dst: Operand? = null,
    val src: Operand? = null,
    val line: Int = -1
)