package ast

/**
 * Represents the different bit-widths that can be used for operands or data.
 * These values are typically used to specify the size of operands in assembly
 * instructions or data definitions.
 */
enum class Bits {
    /**
     * Represents a 32-bit bit-width, typically used for data or operand size specifications
     * in assembly language instructions or data declarations.
     */
    B32,
    /**
     * Represents a 16-bit bit-width, typically used for data or operand size specifications
     * in assembly language instructions or data declarations.
     */
    B16,
    /**
     * Represents an 8-bit bit-width, typically used for data or operand size specifications
     * in assembly language instructions or data declarations.
     */
    B8
}