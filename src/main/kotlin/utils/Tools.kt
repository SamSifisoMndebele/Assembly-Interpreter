package utils

@OptIn(ExperimentalUnsignedTypes::class)
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

@OptIn(ExperimentalUnsignedTypes::class)
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

fun String.removePrefixes(vararg prefix: String): String {
    var string: String = this
    for (pre in prefix) {
        if (string.startsWith(pre)) {
            string = string.substring(pre.length)
        }
    }
    return string
}

fun String.removeSuffixes(vararg suffix: String): String {
    var string: String = this
    for (suf in suffix) {
        if (string.endsWith(suf)) {
            string = string.substring(0, length - suf.length)
        }
    }
    return string
}