@file:OptIn(ExperimentalUnsignedTypes::class)

package utils

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

fun UInt.toUBytes(bits: Int, line: Int = -1): UByteArray {
    return when (bits) {
        8 -> ubyteArrayOf((this and 0xFFu).toUByte())
        16 -> ubyteArrayOf(
            (this and 0xFFu).toUByte(),
            ((this shr 8) and 0xFFu).toUByte()
        )
        32 -> ubyteArrayOf(
            (this and 0xFFu).toUByte(),
            ((this shr 8) and 0xFFu).toUByte(),
            ((this shr 16) and 0xFFu).toUByte(),
            ((this shr 24) and 0xFFu).toUByte()
        )
        else -> error("Unsupported bit size: $bits at line $line")
    }
}