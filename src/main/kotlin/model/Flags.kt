package model

/**
 * Represents the current state of the CPU's status flags.
 *
 * This data class holds boolean values for common CPU flags,
 * which are set or cleared based on the results of arithmetic and logical operations.
 *
 * @property ZF Zero Flag: Set if the result of an arithmetic operation is zero.
 * @property SF Sign Flag: Set if the most significant bit of the result is 1 (indicating a negative number for signed integers).
 * @property CF Carry Flag: Set if an arithmetic operation generates a carry or a borrow out of the most significant bit.
 *                         It is also used by shift and rotate instructions.
 * @property OF Overflow Flag: Set if the integer result is too large a positive number or too small a negative number
 *                           (excluding the sign bit) to fit in the destination operand. This applies to signed arithmetic.
 * @property AF Auxiliary Carry Flag: Set if an arithmetic operation generates a carry or a borrow out of bit 3.
 *                                   This flag is primarily used for Binary Coded Decimal (BCD) arithmetic.
 * @property PF Parity Flag: Set if the least significant byte of the result has an even number of 1 bits.
 *                         If the number of 1 bits is odd, this flag is cleared.
 */
@Suppress("PropertyName")
data class Flags(
    var ZF: Boolean = false, // Zero Flag
    var SF: Boolean = false, // Sign Flag
    var CF: Boolean = false, // Carry Flag
    var OF: Boolean = false, // Overflow Flag
    var AF: Boolean = false, // Auxiliary Carry Flag
    var PF: Boolean = false  // Parity Flag
)