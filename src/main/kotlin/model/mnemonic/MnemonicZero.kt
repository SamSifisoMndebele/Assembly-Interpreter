package model.mnemonic

import model.Bits

/**
 * Represents operations that do not take any operands.
 * These are typically single-byte instructions.
 */
sealed interface MnemonicZero : Mnemonic {
    data object NOP : MnemonicZero {
        override val opcode: UByte = 0x90u
        override val bits: Bits = Bits.B32
    }

    data object RET : MnemonicZero {
        override val opcode: UByte = 0xC3u
        override val bits: Bits = Bits.B32
    }

    data object EXIT : MnemonicZero {
        override val opcode: UByte = 0x00u
        override val bits: Bits = Bits.B32
    }

    data object PUSHA : MnemonicZero {
        override val opcode: UByte = 0x60u
        override val bits: Bits = Bits.B16 // 16-bit registers
    }

    data object PUSHAD : MnemonicZero {
        override val opcode: UByte = 0x60u
        override val bits: Bits = Bits.B32 // 32-bit registers
    }

    data object POPA : MnemonicZero {
        override val opcode: UByte = 0x61u
        override val bits: Bits = Bits.B16 // 16-bit registers
    }

    data object POPAD : MnemonicZero {
        override val opcode: UByte = 0x61u
        override val bits: Bits = Bits.B32 // 32-bit registers
    }

}