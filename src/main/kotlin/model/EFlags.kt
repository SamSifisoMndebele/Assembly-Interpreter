package model

/**
 * Represents the individual CPU status flags (EFlags).
 *
 * Each enum constant represents a specific flag within the EFlags register
 * and holds the bit position of that flag. This enum is used to easily
 * identify and manage the state of these flags.
 */
enum class EFlags(val bit: Int) {
    /** Carry Flag: Set if an arithmetic operation generates a carry or a borrow out of the most significant bit.
     *                         It is also used by shift and rotate instructions. */
    CF(0),   // Carry

    /** Parity Flag: Set if the least significant byte of the result has an even number of 1 bits.
     *                         If the number of 1 bits is odd, this flag is cleared. */
    PF(2),   // Parity

    /** Auxiliary Carry Flag: Set if an arithmetic operation generates a carry or a borrow out of bit 3.
     *                                   This flag is primarily used for Binary Coded Decimal (BCD) arithmetic. */
    AF(4),   // Auxiliary carry

    /** Zero Flag: Set if the result of an arithmetic operation is zero. */
    ZF(6),   // Zero

    /** Sign Flag: Set if the most significant bit of the result is 1 (indicating a negative number for signed integers). */
    SF(7),   // Sign
//    TF(8),   // Trap
//    IF(9),   // Interrupt enable
//    DF(10),  // Direction

    /** Overflow Flag: Set if the integer result is too large a positive number or too small a negative number
     *                           (excluding the sign bit) to fit in the destination operand. This applies to signed arithmetic. */
    OF(11),  // Overflow
//    IOPL(12),// I/O Privilege Level (2 bits)
//    NT(14),  // Nested Task
//    RF(16),  // Resume Flag
//    VM(17),  // Virtual
//    AC(18),  // Alignment Check
//    VIF(19), // Virtual Interrupt Flag
//    VIP(20), // Virtual Interrupt Pending
//    ID(21)   // ID Flag
}