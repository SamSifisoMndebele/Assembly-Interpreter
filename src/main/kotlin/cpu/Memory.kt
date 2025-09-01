package cpu

/**
 * Represents the memory of the CPU.
 *
 * @property memSize The size of the memory in kilobytes. Defaults to 1MB.
 *                   Supports up to 4GB for 32-bit architectures.
 */
@OptIn(ExperimentalUnsignedTypes::class)
class Memory(memSize: Int = 1024) {
    private val mem = UShortArray(memSize * 1024)
    val size = mem.size.toLong()

    /**
     * Reads an 8-bit value from the specified memory address.
     *
     * @param addr The memory address to read from.
     * @return The 8-bit value read from memory.
     */
    fun readByte(addr: Int): UByte {
        val word = mem[addr / 2].toInt()
        return if ((addr and 1) == 0)
            (word and 0xFF).toUByte() // low byte
        else
            (word ushr 8).toUByte() // high byte, shifted down
    }

    /**
     * Writes an 8-bit value to the specified memory address.
     *
     * @param addr The memory address to write to.
     * @param value The 8-bit value to write.
     */
    fun writeByte(addr: Int, value: UByte) {
        val word = mem[addr / 2].toInt()
        mem[addr / 2] =
            if ((addr and 1) == 0) // even address → low byte
                ((word and 0xFF00) or value.toInt()).toUShort()
            else // odd address → high byte
                ((word and 0xFF) or (value.toInt() shl 8)).toUShort()
    }

    /**
     * Reads a 16-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 16-bit value read from memory.
     */
    fun readWord(addr: Int): UShort = mem[addr / 2]

    /**
     * Writes a 16-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 16-bit value to write.
     */
    fun writeWord(addr: Int, value: UShort) {
        mem[addr / 2] = value
    }

    /**
     * Reads a 32-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 32-bit value read from memory.
     */
    fun readDWord(addr: Int): UInt {
        val lo = mem[addr / 2].toInt()
        val hi = mem[addr / 2 + 1].toInt()
        return (lo or (hi shl 16)).toUInt() // Little-endian
    }

    /**
     * Writes a 32-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 32-bit value to write.
     */
    fun writeDWord(addr: Int, value: UInt) {
        mem[addr / 2] = value.toUShort()
        mem[addr / 2 + 1] = (value.toInt() ushr 16).toUShort()
    }
}