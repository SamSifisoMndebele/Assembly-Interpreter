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
    data class Register(val reg: Reg): Operand() {
        override fun toString(): String = reg.name
    }

    /**
     * Represents a 32-bit immediate value operand.
     *
     * @property value The 32-bit integer value.
     */
    data class Immediate(val value: UInt): Operand() {
        override fun toString(): String = value.toString(radix = 16)+"h"
    }

    /**
     * Represents a symbolic label operand. This is typically used for jumps and calls
     * where the target address is represented by a label name.
     *
     * @property name The name of the label.
     */
    data class Label(val name: String): Operand() {
        override fun toString(): String = name
    }

    /**
     * Represents a memory operand, which can be an address formed by a base register and an optional displacement,
     * or just a displacement (absolute address).
     * @property base An optional base register used in address calculation (e.g., `[BX]`, `[EAX + disp]`).
     * @property disp An optional displacement value used in address calculation (e.g., `[1234h]`, `[BX + 8]`).
     */
    data class Memory(
        val base: Reg?,
        val disp: UInt? = null, // Displacement
        val index: Reg? = null, // Index register
        val scale: UInt? = null // Scale for index (1, 2, 4, or 8)
    ) : Operand() {
        override fun toString(): String {
            val parts = mutableListOf<String>()
            if (base != null) {
                parts.add(base.name)
            }
            if (index != null) {
                var indexStr = index.name
                if (scale != null && scale != 1U) {
                    indexStr += "*$scale"
                }
                parts.add(indexStr)
            }
            if (disp != null) {
                val dispStr = if (parts.isNotEmpty() && disp > 0U) "+ ${disp.toString(radix = 16)}h"
                              else if (disp < 0U) "- ${(-disp.toInt()).toString(radix = 16)}h"
                              else "${disp.toString(radix = 16)}h"
                parts.add(dispStr)
            }

            return if (parts.isEmpty()) "" else "[${parts.joinToString(" ")}]"
        }
    }
}