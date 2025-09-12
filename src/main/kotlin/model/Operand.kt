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
     * @property cpuRegister The register being used as an operand.
     */
    data class Register(val cpuRegister: CpuRegister) : Operand {
        override fun toString(): String = cpuRegister.name
    }

    /**
     * Represents a 32-bit immediate value operand.
     *
     * @property value The 32-bit integer value.
     */
    data class Immediate(val value: UInt) : Operand {
        override fun toString(): String = value.toString(radix = 16) + "h"
    }

    /**
     * Represents a symbolic label operand. This is typically used for jumps and calls
     * where the target address is represented by a label name.
     *
     * @property name The name of the label.
     */
    data class Label(val name: String) : Operand {
        override fun toString(): String = name
    }

    /**
     * Represents an identifier operand, often used for symbolic constants or
     * other named values that are not labels or direct memory addresses.
     *
     * @property name The name of the identifier.
     */
    data class Identifier(val name: String) : Operand {
        override fun toString(): String = name
    }

    /**
     * Represents a memory operand, which can be an address formed by a base register and an optional displacement,
     * or just a displacement (absolute address).
     * @property base An optional base register or identifier used in address calculation (e.g., `[BX]`, `[EAX + disp]`, `[myVar]`).
     * @property disp An optional displacement value used in address calculation (e.g., `[1234h]`, `[BX + 8]`).
     */
    data class Memory(
        val base: Operand?, // Can be Register or Identifier
        val disp: UInt? = null // Displacement
    ) : Operand {
        init {
            require(base is Register || base is Identifier || base == null) {
                "Base must be a Register, Identifier, or null"
            }
        }
        override fun toString(): String {
            val baseStr = base?.toString() ?: ""
            val dispStr = disp?.let { d ->
                when {
                    base != null && d > 0U -> "+${d.toString(radix = 16)}h"
                    base != null && d < 0U -> "-${(-d.toInt()).toString(radix = 16)}h"
                    else -> "${d.toString(radix = 16)}h"
                }
            } ?: ""
            return if (baseStr.isEmpty() && dispStr.isEmpty()) "" else "[$baseStr$dispStr]"
        }
    }
}