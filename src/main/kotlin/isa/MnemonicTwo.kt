package isa

/**
 * Represents operations that involve two operands, typically a register and a register/memory location.
 *
 * Note: Some instructions might have variations that take one operand.
 * This interface primarily covers their two-operands forms.
 */
sealed interface MnemonicTwo : Mnemonic {
    /**
     * Moves an 8-bit value from the source operand to the destination operand.
     * The operands can be registers or memory locations.
     */
    data object MOV8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x88u))
        override fun toString(): String = "mov"
    }

    /**
     * Moves a 16-bit value from the source operand to the destination operand.
     * The operands can be registers or memory locations.
     * Requires a 0x66 operand-size prefix (handled in encoder).
     */
    data object MOV16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x89u))
        override fun toString(): String = "mov"
    }

    /**
     * Moves a 32-bit value from the source operand to the destination operand.
     * The operands can be registers or memory locations.
     */
    data object MOV32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x89u))
        override fun toString(): String = "mov"
    }

    /**
     * Exchanges the contents of an 8-bit source operand and an 8-bit destination operand.
     * The operands can be registers or memory locations.
     */
    data object XCHG8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x86u))
        override fun toString(): String = "xchg"
    }

    /**
     * Exchanges the contents of a 16-bit source operand and a 16-bit destination operand.
     * The operands can be registers or memory locations.
     * Requires a 0x66 operand-size prefix (handled in encoder).
     */
    data object XCHG16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x87u))
        override fun toString(): String = "xchg"
    }

    /**
     * Exchanges the contents of a 32-bit source operand and a 32-bit destination operand.
     * The operands can be registers or memory locations.
     */
    data object XCHG32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x87u))
        override fun toString(): String = "xchg"
    }

    /**
     * Moves an 8-bit source operand to a 16-bit destination operand with zero-extension.
     */
    data object MOVZX8to16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xB6u))
        override fun toString(): String = "movzx"
    }

    /**
     * Moves an 8-bit source operand to a 32-bit destination operand with zero-extension.
     */
    data object MOVZX8to32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xB6u))
        override fun toString(): String = "movzx"
    }

    /**
     * Moves a 16-bit source operand to a 32-bit destination operand with zero-extension.
     */
    data object MOVZX16to32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xB7u))
        override fun toString(): String = "movzx"
    }

    /**
     * Moves an 8-bit source operand to a 16-bit destination operand with sign-extension.
     */
    data object MOVSX8to16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xBEu))
        override fun toString(): String = "movsx"
    }

    /**
     * Moves an 8-bit source operand to a 32-bit destination operand with sign-extension.
     */
    data object MOVSX8to32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xBEu))
        override fun toString(): String = "movsx"
    }

    /**
     * Moves a 16-bit source operand to a 32-bit destination operand with sign-extension.
     */
    data object MOVSX16to32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x0Fu, 0xBFu))
        override fun toString(): String = "movsx"
    }

    /**
     * Adds an 8-bit source operand to an 8-bit destination operand.
     */
    data object ADD8 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x00u))
        override fun toString(): String = "add"
    }

    /**
     * Adds a 16-bit source operand to a 16-bit destination operand.
     * Requires a 0x66 operand-size prefix (handled in encoder).
     */
    data object ADD16 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x01u))
        override fun toString(): String = "add"
    }

    /**
     * Adds a 32-bit source operand to a 32-bit destination operand.
     */
    data object ADD32 : MnemonicTwo {
        override val encoding = Encoding(opcode = listOf(0x01u))
        override fun toString(): String = "add"
    }
}