package cpu

import kotlin.experimental.and

/**
 * Represents the memory of the CPU.
 *
 * @property memSize The size of the memory in kilobytes. Defaults to 1MB.
 *                   Supports up to 4GB for 32-bit architectures.
 */
class Memory(memSize: Int = 1024) {
    private val mem = ByteArray(memSize * 1024)
    val size = mem.size.toLong()

    /**
     * Reads an 8-bit value from the specified memory address.
     *
     * @param addr The memory address to read from.
     * @return The 8-bit value read from memory.
     */
    fun readByte(addr: Long): Short = (mem[(addr and 0xFFFFFFFFL).toInt()].toInt() and 0xFF).toShort()
    /**
     * Writes an 8-bit value to the specified memory address.
     *
     * @param addr The memory address to write to.
     * @param value The 8-bit value to write.
     */
    fun writeByte(addr: Long, value: Short) {
        mem[(addr and 0xFFFFFFFFL).toInt()] = (value.toInt() and 0xFF).toByte()
    }

    /**
     * Reads a 16-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 16-bit value read from memory.
     */
    fun readWord(addr: Long): Int {
        val lo = readByte(addr).toInt()
        val hi = readByte(addr + 1).toInt()
        return lo or (hi shl 8) // Little-endian
    }

    /**
     * Writes a 16-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 16-bit value to write.
     */
    fun writeWord(addr: Long, value: Int) {
        writeByte(addr, (value and 0xFF).toShort())          // Little-endian
        writeByte(addr + 1, ((value ushr 8) and 0xFF).toShort())
    }

    /**
     * Reads a 32-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 32-bit value read from memory.
     */
    fun readDWord(addr: Long): Long {
        val b0 = readByte(addr).toLong()
        val b1 = readByte(addr + 1).toLong()
        val b2 = readByte(addr + 2).toLong()
        val b3 = readByte(addr + 3).toLong()
        return b0 or (b1 shl 8) or (b2 shl 16) or (b3 shl 24) // Little-endian
    }
    /**
     * Writes a 32-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 32-bit value to write.
     */
    fun writeDWord(addr: Long, value: Long) {
        writeByte(addr, (value and 0xFF).toShort())                  // Little-endian
        writeByte(addr + 1, ((value ushr 8) and 0xFF).toShort())
        writeByte(addr + 2, ((value ushr 16) and 0xFF).toShort())
        writeByte(addr + 3, ((value ushr 24) and 0xFF).toShort())
    }
}