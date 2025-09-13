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
        val base: Register? = null,
        val index: Register? = null,
        val scale: UInt = 1u,
        val disp: Long? = null
    ) : Operand {
        init {
            require(base != null || index != null || disp != null) {
                "At least one of base, index, or disp must be provided"
            }
            require(scale in listOf(1u, 2u, 4u, 8u)) {
                "Scale must be 1, 2, 4, or 8"
            }
            require(scale == 1u || index != null) {
                "Scale requires an index register"
            }
        }
        override fun toString(): String {
            val parts = mutableListOf<String>()
            base?.let { parts += it.cpuRegister.name }
            index?.let {
                val idxPart = if (scale == 1u) it.cpuRegister.name else "${it.cpuRegister.name}*${scale}"
                parts += if (parts.isEmpty()) idxPart else "+$idxPart"
            }
            disp?.let {
                when {
                    it > 0 -> parts += (if (parts.isEmpty()) "" else "+") + it.toString(radix = 16) + "h"
                    it < 0 -> parts += "-" + (-it).toString(radix = 16) + "h"
                }
            }

            return "[${parts.joinToString("")}]"
        }
    }
}