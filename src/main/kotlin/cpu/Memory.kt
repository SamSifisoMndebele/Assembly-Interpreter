package cpu

/**
 * Represents the memory of the CPU.
 *
 * @property memSize The size of the memory in kilobytes. Defaults to 256KB.
 *                   Supports up to 4GB for 32-bit architectures.
 */
class Memory(memSize: Int = 256) {
    private val mem = ByteArray(memSize * 1024)
    val size = mem.size.toLong()

    /**
     * Reads an 8-bit value from the specified memory address.
     *
     * @param addr The memory address to read from.
     * @return The 8-bit value read from memory.
     */
    fun readByte(addr: Long): Long = mem[(addr and 0xFFFFFFFFL).toInt()].toLong() and 0xFFL
    /**
     * Writes an 8-bit value to the specified memory address.
     *
     * @param addr The memory address to write to.
     * @param value The 8-bit value to write.
     */
    fun writeByte(addr: Long, value: Long) {
        mem[(addr and 0xFFFFFFFFL).toInt()] = (value and 0xFFL).toByte()
    }

    /**
     * Reads a 16-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 16-bit value read from memory.
     */
    fun readWord(addr: Long): Long {
        val lo = readByte(addr)
        val hi = readByte(addr + 1)
        return lo or (hi shl 8) // Little-endian
    }

    /**
     * Writes a 16-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 16-bit value to write.
     */
    fun writeWord(addr: Long, value: Long) {
        writeByte(addr, value and 0xFFL)          // Little-endian
        writeByte(addr + 1, (value ushr 8) and 0xFFL)
    }

    /**
     * Reads a 32-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 32-bit value read from memory.
     */
    fun readDWord(addr: Long): Long {
        val b0 = readByte(addr)
        val b1 = readByte(addr + 1)
        val b2 = readByte(addr + 2)
        val b3 = readByte(addr + 3)
        return b0 or (b1 shl 8) or (b2 shl 16) or (b3 shl 24) // Little-endian
    }
    /**
     * Writes a 32-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 32-bit value to write.
     */
    fun writeDWord(addr: Long, value: Long) {
        writeByte(addr, value and 0xFFL)                  // Little-endian
        writeByte(addr + 1, (value ushr 8) and 0xFFL)
        writeByte(addr + 2, (value ushr 16) and 0xFFL)
        writeByte(addr + 3, (value ushr 24) and 0xFFL)
    }


}