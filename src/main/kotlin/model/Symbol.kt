package model

class Symbol(
    val type: String,
    val length: Int,
    val address: Long,
) {
    val size: Int = when (type.uppercase()) {
        "BYTE", "DB" -> 1
        "WORD", "DW" -> 2
        "DWORD", "DD" -> 4
        "QWORD", "DQ" -> 8
        else -> error("Unknown data directive type: $type")
    }

    override fun toString(): String = "$type $address | $length"
}