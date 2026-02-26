package isa

/**
 * Represents operations that involve two operands, typically a register and a register/memory location.
 *
 * Note: Some instructions might have variations that take one operand.
 * This interface primarily covers their two-operands forms.
 */
sealed interface MnemonicTwo : Mnemonic {
    /* ------------------- MOV ------------------- */
    data object MOV8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x88u))
    }
    data object MOV16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x89u))
    }
    data object MOV32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x89u))
    }

    /* ------------------- XCHG ------------------- */
    data object XCHG8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x86u))
    }
    data object XCHG16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x87u))
    }
    data object XCHG32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x87u))
    }

    /* ------------------- MOVZX (Zero-Extend) ------------------- */
    data object MOVZX_16_8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xB6u))
    }
    data object MOVZX_32_8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xB6u))
    }
    data object MOVZX_32_16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xB7u))
    }

    /* ------------------- MOVSX (Sign-Extend) ------------------- */
    data object MOVSX_16_8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xBEu))
    }
    data object MOVSX_32_8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xBEu))
    }
    data object MOVSX_32_16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xBFu))
    }

    // Arithmetic Instructions
    data object ADD8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x00u))
    }
    data object ADD16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x01u))
    }
    data object ADD32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x01u))
    }
}