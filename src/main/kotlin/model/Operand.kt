package model

/**
 * Represents the different types of operands that can be used in assembly instructions.
 * Operands can be registers, immediate values (8-bit, 16-bit, or 32-bit),
 * symbolic labels, or memory addresses.
 */
sealed class Operand {
    /**
     * Represents a register operand.
     *
     * @property reg The register being used as an operand.
     */
    data class RegOp(val reg: Reg): Operand()

    /**
     * Represents an 8-bit immediate value operand.
     *
     * @property value The 8-bit integer value.
     */
    data class Imm8Op(val value: Byte): Operand()

    /**
     * Represents a 16-bit immediate value operand.
     *
     * @property value The 16-bit integer value.
     */
    data class Imm16Op(val value: Short): Operand()

    /**
     * Represents a 32-bit immediate value operand.
     *
     * @property value The 32-bit integer value.
     */
    data class Imm32Op(val value: Int): Operand()

    /**
     * Represents a symbolic label operand. This is typically used for jumps and calls
     * where the target address is represented by a label name.
     *
     * @property name The name of the label.
     */
    data class LabelOp(val name: String): Operand() // For symbolic labels

    /**
     * Represents a memory operand, which can be an address formed by a base register and an optional displacement,
     * or just a displacement (absolute address).
     * @property base An optional base register used in address calculation (e.g., `[BX]`, `[EAX + disp]`).
     * @property disp An optional displacement value used in address calculation (e.g., `[1234h]`, `[BX + 8]`).
     */
    data class MemOp(val base: Reg?, val disp: Int?): Operand() // [base + disp] or [disp]
}