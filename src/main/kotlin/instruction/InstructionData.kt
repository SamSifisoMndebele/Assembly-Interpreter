package instruction

@OptIn(ExperimentalUnsignedTypes::class)
class InstructionData(
    val label: String,
    val type: String, // WORD, DWORD, etc.
    val data: UByteArray?, // Or String, depending on how you want to handle various data types like DB, DW, DD
    override val line: Int
) : Instruction {
    override fun encode(): UByteArray {
        TODO("Not yet implemented")
    }

    override fun toString(): String {
        val dataStr = data?.joinToString(separator = ",") {
            byte -> byte.toString(16) + "h"
        } ?: "?"
        return "$line: $label $type $dataStr"
    }
}