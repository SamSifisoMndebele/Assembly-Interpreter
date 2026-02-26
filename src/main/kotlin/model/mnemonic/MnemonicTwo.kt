package model.mnemonic

import model.Bits

/**
 * Represents operations that involve two operands, typically a register and a register/memory location.
 *
 * Note: Some instructions might have variations that take one operand.
 * This interface primarily covers their two-operands forms.
 */
sealed interface MnemonicTwo : Mnemonic {
    /* ------------------- MOV ------------------- */
    data class MOV(
        val destBits: Bits,        // 8, 16, or 32
        val srcBits: Bits          // 8, 16, or 32
    ) : MnemonicTwo {
        init {
            require(destBits == Bits.B8 || destBits == Bits.B16 || destBits == Bits.B32) {
                "Invalid destBits value: $destBits"
            }
            require(srcBits == destBits) {
                "Source and destination bits must match: dest=$destBits src=$srcBits"
            }
        }

        override val opcode: UByte
            get() = when (destBits) {
                Bits.B8 -> 0x88u   // r/m8 ← r8
                Bits.B16, Bits.B32 -> 0x89u  // r/m16/32 ← r16/32 (0x66 prefix handled elsewhere for 16-bit)
                else -> error("Invalid MOV combination: dest=$destBits src=$srcBits")
            }

        override val bits: Bits = destBits
    }

    /* ------------------- XCHG ------------------- */
    data class XCHG(
        val destBits: Bits,        // 8, 16, or 32
        val srcBits: Bits          // 8, 16, or 32
    ) : MnemonicTwo {
        init {
            require(destBits == Bits.B8 || destBits == Bits.B16 || destBits == Bits.B32) {
                "Invalid destBits value: $destBits"
            }
            require(srcBits == destBits) {
                "Source and destination bits must match: dest=$destBits src=$srcBits"
            }
        }

        override val opcode: UByte
            get() = when (destBits) {
                Bits.B8 -> 0x86u   // XCHG r/m8, r8
                Bits.B16, Bits.B32 -> 0x87u  // XCHG r/m16/32, r16/32
                else -> error("Invalid XCHG combination: dest=$destBits src=$srcBits")
            }

        override val bits: Bits = destBits
    }

    /* ------------------- MOVZX (Zero-Extend) ------------------- */
    data class MOVZX(
        val destBits: Bits,        // 16 or 32 (destination register size)
        val srcBits: Bits          // 8 or 16 (source operand size)
    ) : MnemonicTwo {
        init {
            require(srcBits == Bits.B8 || srcBits == Bits.B16) {
                "Invalid srcBits value: $srcBits"
            }
            require(destBits == Bits.B16 || destBits == Bits.B32) {
                "Invalid destBits value: $destBits"
            }
        }
        override val opcode: UByte
            get() = when {
                srcBits == Bits.B8  -> 0xB6u // 8 → 16/32
                srcBits == Bits.B16 && destBits == Bits.B32 -> 0xB7u // 16 → 32
                else -> error("Invalid MOVZX combination: dest=$destBits src=$srcBits")
            }
        override val bits: Bits = destBits
    }

    /* ------------------- MOVSX (Sign-Extend) ------------------- */
    data class MOVSX(
        val destBits: Bits,        // 16 or 32 (destination register size)
        val srcBits: Bits          // 8 or 16 (source operand size)
    ) : MnemonicTwo {
        init {
            require(srcBits == Bits.B8 || srcBits == Bits.B16) {
                "Invalid srcBits value: $srcBits"
            }
            require(destBits == Bits.B16 || destBits == Bits.B32) {
                "Invalid destBits value: $destBits"
            }
        }
        override val opcode: UByte
            get() = when {
                srcBits == Bits.B8  -> 0xBEu // 8 → 16/32
                srcBits == Bits.B16 && destBits == Bits.B32 -> 0xBFu // 16 → 32
                else -> error("Invalid MOVSX combination: dest=$destBits src=$srcBits")
            }
        override val bits: Bits = destBits
    }


//        data object MOVR  : OperationTwo { override val opcode: UByte = 0x8Bu } // Move r/m32 -> r32 (reverse)
//        data object MOVZXR: OperationOne { override val opcode: UByte = 0xB7u } // Move r/m16 -> r32 with zero-extend
//        data object MOVSXR: OperationOne { override val opcode: UByte = 0xBFu } // Move r/m16 -> r32 with sign-extend


    // Arithmetic Instructions
    data class ADD(
        val destBits: Bits,  // 8, 16, or 32 (destination operand size)
        val srcBits: Bits    // 8, 16, or 32 (source operand size)
    ) : MnemonicTwo {

        init {
            require(destBits in setOf(Bits.B8, Bits.B16, Bits.B32)) {
                "Invalid destBits value for ADD: $destBits"
            }
            require(srcBits in setOf(Bits.B8, Bits.B16, Bits.B32)) {
                "Invalid srcBits value for ADD: $srcBits"
            }
            require(destBits == srcBits) {
                "Source and destination bits must match: dest=$destBits src=$srcBits"
            }
        }

        /**
         * Primary opcode depends on operand size.
         * Direction (reg → r/m or r/m → reg) is determined by the D-bit in ModR/M (handled in encoder).
         *
         * - 8-bit:  0x00 (r/m8 ← r8) or 0x02 (r8 ← r/m8, D=1)
         * - 16-bit: 0x01 (r/m16 ← r16, 0x66 prefix required) or 0x03 (r16 ← r/m16, D=1)
         * - 32-bit: 0x01 (r/m32 ← r32) or 0x03 (r32 ← r/m32, D=1)
         */
        override val opcode: UByte
            get() = when (destBits) {
                Bits.B8  -> 0x00u
                Bits.B16, Bits.B32 -> 0x01u
                else -> error("Invalid bits value: $destBits")
            }

        override val bits: Bits = destBits

        override fun toString(): String = "ADD ${destBits.name} ← ${srcBits.name}"
    }

}