package model

sealed class Operand {
    data class RegOp(val reg: Reg): Operand()
    data class ImmOp(val value: Int): Operand() // 16-bit immediate
    data class LabelOp(val name: String): Operand() // For symbolic labels
    data class MemOp(val base: Reg?, val disp: Int?): Operand() // [base + disp] or [disp]
}