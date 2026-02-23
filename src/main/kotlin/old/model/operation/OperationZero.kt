package old.model.operation

import old.model.Bits

/**
 * Represents operations that do not take any operands.
 * These are typically single-byte instructions.
 */
sealed interface OperationZero : Operation {
    data object NOP : OperationZero {
        override val opcode: UByte = 0x90u
        override val bits: Bits = Bits.NONE
    }

    data object RET : OperationZero {
        override val opcode: UByte = 0xC3u
        override val bits: Bits = Bits.NONE
    }

    data object EXIT : OperationZero {
        override val opcode: UByte = 0x00u
        override val bits: Bits = Bits.NONE
    }

    data object PUSHA : OperationZero {
        override val opcode: UByte = 0x60u
        override val bits: Bits = Bits.B16 // 16-bit registers
    }

    data object PUSHAD : OperationZero {
        override val opcode: UByte = 0x60u
        override val bits: Bits = Bits.B32 // 32-bit registers
    }

    data object POPA : OperationZero {
        override val opcode: UByte = 0x61u
        override val bits: Bits = Bits.B16 // 16-bit registers
    }

    data object POPAD : OperationZero {
        override val opcode: UByte = 0x61u
        override val bits: Bits = Bits.B32 // 32-bit registers
    }

//        data object PUSHF : OperationZero { override val opcode: UByte = 0x9Cu }
//        data object POPF : OperationZero { override val opcode: UByte = 0x9Du }
//        data object HLT : OperationZero { override val opcode: UByte = 0xF4u }
//        data object CMC : OperationZero { override val opcode: UByte = 0xF5u }
//        data object STC : OperationZero { override val opcode: UByte = 0xF9u }
//        data object CLC : OperationZero { override val opcode: UByte = 0xF8u }
//        data object IRET : OperationZero { override val opcode: UByte = 0xCFu }
//        data object INT3 : OperationZero { override val opcode: UByte = 0xCCu }
//        data object WAIT : OperationZero { override val opcode: UByte = 0x9Bu }
//        data object CLD : OperationZero { override val opcode: UByte = 0xFCu }
//        data object STD : OperationZero { override val opcode: UByte = 0xFDu }
//        data object CLI : OperationZero { override val opcode: UByte = 0xFAu }
//        data object STI : OperationZero { override val opcode: UByte = 0xFBu }
//        data object CMPS : OperationZero { override val opcode: UByte = 0xA6u }
//        data object SCAS : OperationZero { override val opcode: UByte = 0xAEu }
//        data object LODS : OperationZero { override val opcode: UByte = 0xACu }
//        data object STOS : OperationZero { override val opcode: UByte = 0xAAu }

}