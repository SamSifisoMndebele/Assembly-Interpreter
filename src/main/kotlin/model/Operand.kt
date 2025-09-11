package model

/**
 * Represents the different types of operands that can be used in assembly instructions.
 * Operands can be registers, immediate values (8-bit, 16-bit, or 32-bit),
 * symbolic labels, or memory addresses.
 */
sealed interface Operand {
    /**
     * Represents a register operand.
     *
     * @property register The register being used as an operand.
     */
    data class Register(val register: model.Register): Operand {
        override fun toString(): String = register.name
    }

    /**
     * Represents a 32-bit immediate value operand.
     *
     * @property value The 32-bit integer value.
     */
    data class Immediate(val value: UInt): Operand {
        override fun toString(): String = value.toString(radix = 16)+"h"
    }

    /**
     * Represents a symbolic label operand. This is typically used for jumps and calls
     * where the target address is represented by a label name.
     *
     * @property name The name of the label.
     */
    data class Label(val name: String): Operand {
        override fun toString(): String = name
    }

    /**
     * Represents a memory operand, which can be an address formed by a base register and an optional displacement,
     * or just a displacement (absolute address).
     * @property base An optional base register used in address calculation (e.g., `[BX]`, `[EAX + disp]`).
     * @property disp An optional displacement value used in address calculation (e.g., `[1234h]`, `[BX + 8]`).
     */
    data class Memory(
        val base: model.Register?,
        val disp: UInt? = null // Displacement
    ) : Operand {
        override fun toString(): String {
            val baseStr = base?.name ?: ""
            val dispStr = disp?.let { d ->
                when {
                    base != null && d > 0U -> "+ ${d.toString(radix = 16)}h"
                    base != null && d < 0U -> "- ${(-d.toInt()).toString(radix = 16)}h"
                    else -> "${d.toString(radix = 16)}h"
                }
            } ?: ""
            return if (baseStr.isEmpty() && dispStr.isEmpty()) "" else "[$baseStr$dispStr]"
        }
    }
}