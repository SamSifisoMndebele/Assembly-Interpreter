package model.operand

/**
 * Represents a symbolic label operand. This is typically used for jumps and calls
 * where the target address is represented by a label name.
 *
 * @property name The name of the label.
 */
data class Label(val name: String) : Operand {
    override fun toString(): String = name
}