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
    fun read8(addr: Long): Long = mem[(addr and 0xFFFFFFFFL).toInt()].toLong() and 0xFFL
    /**
     * Writes an 8-bit value to the specified memory address.
     *
     * @param addr The memory address to write to.
     * @param value The 8-bit value to write.
     */
    fun write8(addr: Long, value: Long) {
        mem[(addr and 0xFFFFFFFFL).toInt()] = (value and 0xFFL).toByte()
    }

    /**
     * Reads a 16-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 16-bit value read from memory.
     */
    fun read16(addr: Long): Long {
        val lo = read8(addr)
        val hi = read8(addr + 1)
        return lo or (hi shl 8) // Little-endian
    }

    /**
     * Writes a 16-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 16-bit value to write.
     */
    fun write16(addr: Long, value: Long) {
        write8(addr, value and 0xFFL)          // Little-endian
        write8(addr + 1, (value ushr 8) and 0xFFL)
    }

    /**
     * Reads a 32-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 32-bit value read from memory.
     */
    fun read32(addr: Long): Long {
        val b0 = read8(addr)
        val b1 = read8(addr + 1)
        val b2 = read8(addr + 2)
        val b3 = read8(addr + 3)
        return b0 or (b1 shl 8) or (b2 shl 16) or (b3 shl 24) // Little-endian
    }
    /**
     * Writes a 32-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 32-bit value to write.
     */
    fun write32(addr: Long, value: Long) {
        write8(addr, value and 0xFFL)                  // Little-endian
        write8(addr + 1, (value ushr 8) and 0xFFL)
        write8(addr + 2, (value ushr 16) and 0xFFL)
        write8(addr + 3, (value ushr 24) and 0xFFL)
    }
}