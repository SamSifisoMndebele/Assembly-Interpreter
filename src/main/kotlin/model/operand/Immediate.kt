package model.operand

/**
 * Represents a 32-bit immediate value operand.
 *
 * @property value The 32-bit integer value.
 */
data class Immediate(val value: UInt) : Operand {
    override fun toString(): String = value.toString(16)
}