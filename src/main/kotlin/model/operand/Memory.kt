package model.operand

import kotlin.collections.plusAssign

/**
 * Represents a memory operand, which can be an address formed by a base register, an optional index register,
 * an optional scale, and an optional displacement, or just a displacement (absolute address).
 *
 * The general form of a memory operand is: `[base + index*scale + displacement]`
 *
 * @property base An optional base register or identifier used in address calculation (e.g., `[BX]`, `[EAX + disp]`, `[myVar]`).
 * @property index An optional index register used in address calculation (e.g., `[ESI]`, `[EBX + EDI*2]`).
 * @property scale An optional scale factor for the index register, must be 1, 2, 4, or 8 (e.g., `[EAX*4]`). Defaults to 1 if an index is present.
 * @property disp An optional displacement value used in address calculation (e.g., `[1234h]`, `[BX + 8]`).
 * @throws IllegalArgumentException if none of base, index, or disp are provided, or, if scale is not 1, 2, 4, or 8, or, if scale is specified without an index register (unless scale is 1).
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
        base?.let { parts.plusAssign(it.cpuRegister.name) }
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