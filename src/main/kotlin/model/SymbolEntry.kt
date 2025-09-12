package model

@OptIn(ExperimentalUnsignedTypes::class)
class SymbolEntry(
    val name: String,
    val address: Long,
    val type: String, // WORD, DWORD, etc.
    val data: UByteArray?,
    val line: Int
) {
    val size: Int = when (type) {
        "BYTE", "DB" -> 1
        "WORD", "DW" -> 2
        "DWORD", "DD" -> 4
        "QWORD", "DQ" -> 8
        else -> error("Unknown data directive type: $type at line $line")
    }
    val length: Int = size * (data?.size ?: 1)

    override fun toString(): String {
        if ((type == "BYTE" || type == "DB") && data?.firstOrNull()?.toInt()?.toChar()?.isLetter() == true) {
            val string = data.map { it.toInt().toChar() }.toString()
            return "$line: $name $type \"$string\""
        }
        val array = data?.joinToString(separator = ",") {
                byte -> byte.toString(16) + "h"
        } ?: "?"
        return "$line: $name $type $array"
    }
}