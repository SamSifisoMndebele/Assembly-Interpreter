package isa

/**
 * Represents operations that do not take any operands.
 * These are typically single-byte instructions.
 */
sealed interface MnemonicZero : Mnemonic {
    data object NOP : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x90u))
    }

    data object RET : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0xC3u))
    }

    data object EXIT : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x00u))
    }

    data object PUSHA : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x60u))
    }

    data object PUSHAD : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x60u))
    }

    data object POPA : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x61u))
    }

    data object POPAD : MnemonicZero {
        override val encoding = Encoding(opcode = listOf(0x61u))
    }

}