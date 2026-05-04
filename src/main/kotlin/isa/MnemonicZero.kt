package isa

import kotlin.reflect.jvm.jvmName

/**
 * Represents operations that do not take any operands.
 * These are typically single-byte instructions.
 */
sealed interface MnemonicZero : Mnemonic {
    data object NOP : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x90u))
        override fun toString(): String = "nop"
    }

    data object RET : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0xC3u))
        override fun toString(): String = "ret"
    }

    data object EXIT : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x00u))
        override fun toString(): String = "exit"
    }

    data object PUSHA : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x60u))
        override fun toString(): String = "pusha"
    }

    data object PUSHAD : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x60u))
        override fun toString(): String = "pushad"
    }

    data object POPA : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x61u))
        override fun toString(): String = "popa"
    }

    data object POPAD : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x61u))
        override fun toString(): String = "popad"
    }

}