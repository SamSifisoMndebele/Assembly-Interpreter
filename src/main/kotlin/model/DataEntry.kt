package model

@OptIn(ExperimentalUnsignedTypes::class)
class DataEntry(
    val name: String,
    val type: String, // WORD, DWORD, etc.
    val bytes: UByteArray?,
    val line: Int
) {
    val size: Int = when (type.uppercase()) {
        "BYTE", "DB" -> 1
        "WORD", "DW" -> 2
        "DWORD", "DD" -> 4
        "QWORD", "DQ" -> 8
        else -> error("Unknown data directive type: $type at line $line")
    }
    val length: Int = bytes?.size ?: 1

    override fun toString(): String {
        if ((type == "BYTE" || type == "DB") && this@DataEntry.bytes?.firstOrNull()?.toInt()?.toChar()?.isLetter() == true) {
            val string = bytes.map { it.toInt().toChar() }.toString()
            return "$line: $name $type \"$string\""
        }
        val array = bytes?.joinToString(separator = ",") {
                byte -> byte.toString(16) + "h"
        } ?: "?"
        return "$line: $name $type $array"
    }
}