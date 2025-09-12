package cpu

/**
 * Represents the memory of the CPU. This implementation focuses on byte-addressable memory
 * with support for reading and writing 8-bit, 16-bit, and 32-bit values in little-endian format.
 *
 * @property bytes The size of the memory in bytes. Must be between 32 bytes and 8GB (8,589,934,592 bytes).
 *                 Defaults to 64KB (65,536 bytes).
 */
@OptIn(ExperimentalUnsignedTypes::class)
class Memory(bytes: Long = 65_536) {
    init {
        require(bytes in 32..8_589_934_592) {
            "Memory size must be between 32 bytes and 8GB"
        }
    }

    /**
     * Secondary constructor to initialize memory with a size in kilobytes.
     *
     * @param kb The size of the memory in kilobytes. Must be between 1KB and 8GB (8,388,608 kilobytes).
     */
    @Suppress("unused")
    constructor(kb: Int) : this(kb * 1024L)

    private val mem = UIntArray((bytes / 4).toInt()) // each element is 32-bit (4 bytes)
    val bytes = bytes

    companion object {
        const val RESET = "\u001B[0m"
        const val RED = "\u001B[31m"
        const val GREEN = "\u001B[32m"
        const val YELLOW = "\u001B[33m"
        const val BLUE = "\u001B[34m"
        const val BOLD = "\u001B[1m"
    }

    /**
     * Reads an 8-bit value from the specified memory address.
     *
     * @param addr The memory address to read from.
     * @return The 8-bit value read from memory.
     */
    fun readByte(addr: Long): UByte {
        val index = (addr / 4).toInt()
        val shift = (addr % 4).toInt() * 8
        return ((mem[index].toInt() ushr shift) and 0xFF).toUByte()
    }

    /**
     * Writes an 8-bit value to the specified memory address.
     *
     * @param addr The memory address to write to.
     * @param value The 8-bit value to write.
     */
    fun writeByte(addr: Long, value: UByte) {
        val index = (addr / 4).toInt()
        val shift = (addr % 4).toInt() * 8
        val mask = (0xFFu shl shift).inv()
        mem[index] = (mem[index] and mask) or (value.toUInt() shl shift)
    }

    /**
     * Reads a 16-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 16-bit value read from memory.
     */
    fun readWord(addr: Long): UShort {
        val lo = readByte(addr).toUInt()
        val hi = readByte(addr + 1).toUInt()
        return ((hi shl 8) or lo).toUShort()
    }

    /**
     * Writes a 16-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 16-bit value to write.
     */
    fun writeWord(addr: Long, value: UShort) {
        writeByte(addr, (value and 0xFFu).toUByte())
        writeByte(addr + 1, ((value.toInt() ushr 8) and 0xFF).toUByte())
    }

    /**
     * Reads a 32-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 32-bit value read from memory.
     */
    fun readDWord(addr: Long): UInt = mem[(addr / 4).toInt()]

    /**
     * Writes a 32-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 32-bit value to write.
     */
    fun writeDWord(addr: Long, value: UInt) {
        mem[(addr / 4).toInt()] = value
    }

    /**
     * Prints the memory content in a human-readable format, allowing for a specified range and number of rows.
     *
     * @param start The starting memory address to print. Defaults to 0.
     * @param end The ending memory address to print (exclusive). Defaults to the total size of the memory.
     * @param rows The maximum number of rows to print. Defaults to the number of rows required to display the specified range.
     *             It will be coerced to be at most the total number of rows available in memory.
     */
    fun printMemory(start: Long = 0, end: Long = bytes, rows: Int = ((end - start + 15) / 16).toInt()) {
        println("${BLUE}Memory Dump:$RESET")
        println("${BLUE}Address    | ${(0 until 16).joinToString(" ") { "%02X".format(it) }}$RESET")
        println("${BLUE}-----------|-${"-".repeat(47)}$RESET")
        val actualRows = rows.coerceAtMost(((bytes + 15) / 16).toInt().coerceAtLeast(1))
        val bold = BOLD
        for (i in 0 until actualRows) {
            val baseAddr = start + i * 16L
            print("$YELLOW%08X$RESET   | ".format(baseAddr))
            for (j in 0 until 16) {
                val addr = baseAddr + j
                if (addr < end && addr < bytes) {
                    val byteValue = readByte(addr)
                    val color = if (byteValue == 0u.toUByte()) "" else "$GREEN$bold"
                    print("$color%02X $RESET".format(byteValue.toShort()))
                } else {
                    print("   ")
                }
            }
            println()
        }
    }
}

// Testing
fun main() {
    val memory = Memory(64L)

    // Example of writing and reading memory
    memory.writeByte(0x00, 0xABu)
    memory.writeByte(0x01, 0xCDu)
    memory.writeWord(0x02, 0xEF01u)
    memory.writeDWord(0x04, 0x12345678u)

    memory.printMemory()

    val reset = "\u001B[0m"
    val red = "\u001B[31m"
    println("$red Byte at 00: %02X $reset".format(memory.readByte(0x00).toShort()))
    println("$red Word at 02: %04X $reset".format(memory.readWord(0x02).toShort()))
    println("$red DWord at 04: %08X $reset".format(memory.readDWord(0x04).toShort()))
}