package ast

/**
 * Represents the different types of operands that can be used in assembly instructions.
 * Operands can be registers, immediate values (8-bit, 16-bit, or 32-bit),
 * symbolic labels, or memory addresses.
 */
sealed interface Operand {
    override fun toString(): String


    /**
     * Represents a 32-bit immediate value operand.
     *
     * @property value The 32-bit integer value.
     */
    data class Immediate(val value: UInt, val bits: Bits = if (value.toInt() in -128..127) Bits.B16 else Bits.B32) : Operand {
        override fun toString(): String = value.toString(16)
    }
}