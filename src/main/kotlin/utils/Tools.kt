@file:OptIn(ExperimentalUnsignedTypes::class)

package utils

import model.Bits

fun UInt.toUBytes(type: String, line: Int): UByteArray {
    return when (type.uppercase()) {
        "BYTE", "DB" -> ubyteArrayOf((this and 0xFFu).toUByte())
        "WORD", "DW" -> ubyteArrayOf(
            (this and 0xFFu).toUByte(),
            ((this shr 8) and 0xFFu).toUByte()
        )
        "DWORD", "DD" -> ubyteArrayOf(
            (this and 0xFFu).toUByte(),
            ((this shr 8) and 0xFFu).toUByte(),
            ((this shr 16) and 0xFFu).toUByte(),
            ((this shr 24) and 0xFFu).toUByte()
        )
        "QWORD", "DQ" -> {
            val value = this.toULong()
            ubyteArrayOf(
                (value and 0xFFuL).toUByte(),
                ((value shr 8) and 0xFFuL).toUByte(),
                ((value shr 16) and 0xFFuL).toUByte(),
                ((value shr 24) and 0xFFuL).toUByte(),
                ((value shr 32) and 0xFFuL).toUByte(),
                ((value shr 40) and 0xFFuL).toUByte(),
                ((value shr 48) and 0xFFuL).toUByte(),
                ((value shr 56) and 0xFFuL).toUByte()
            )
        }
        else -> error("Unknown data directive type: $type at line $line")
    }
}

fun UInt.toUBytes(bits: Bits = Bits.B32, line: Int = -1): UByteArray {
    return when (bits) {
        Bits.B8 -> ubyteArrayOf((this and 0xFFu).toUByte())
        Bits.B16 -> ubyteArrayOf(
            (this and 0xFFu).toUByte(),
            ((this shr 8) and 0xFFu).toUByte()
        )
        Bits.B32 -> ubyteArrayOf(
            (this and 0xFFu).toUByte(),
            ((this shr 8) and 0xFFu).toUByte(),
            ((this shr 16) and 0xFFu).toUByte(),
            ((this shr 24) and 0xFFu).toUByte()
        )
//        Bits.B64 -> {
//            val value = this.toULong()
//            ubyteArrayOf(
//                (value and 0xFFuL).toUByte(),
//                ((value shr 8) and 0xFFuL).toUByte(),
//                ((value shr 16) and 0xFFuL).toUByte(),
//                ((value shr 24) and 0xFFuL).toUByte(),
//                ((value shr 32) and 0xFFuL).toUByte(),
//                ((value shr 40) and 0xFFuL).toUByte(),
//                ((value shr 48) and 0xFFuL).toUByte(),
//                ((value shr 56) and 0xFFuL).toUByte()
//            )
//        }
//        Bits.NONE -> error("Unknown data directive bits: $bits at line $line")
    }
}