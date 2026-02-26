package isa

data class Encoding(
    val opcode: List<UByte>,
    val modRmExtension: Int? = null
)