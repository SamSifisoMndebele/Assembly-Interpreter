package model.operand

/**
 * Represents an identifier operand, often used for symbolic constants or
 * other named values that are not labels or direct memory addresses.
 *
 * @property name The name of the identifier.
 */
data class Identifier(val name: String) : Operand {
    override fun toString(): String = name
}