package cpu

/**
 * Represents the memory of the CPU. This implementation focuses on byte-addressable memory
 * with support for reading and writing 8-bit, 16-bit, and 32-bit values in little-endian format.
 *
 * @property bytes The size of the memory in bytes. Defaults to 1MB (1,048,576 bytes).
 *                   The minimum configurable size is 32 bytes, and the maximum is 8GB (8,589,934,592 bytes).
 *                   A secondary constructor is available to initialize memory using kilobytes (KB).
 */
@OptIn(ExperimentalUnsignedTypes::class)
class Memory(bytes: Long = 1_048_576) {
    init {
        require(bytes in 32..8_589_934_592) {
            "Memory size must be between 32 bytes and 8GB"
        }
    }
    @Suppress("unused")
    constructor(kb: Int) : this(kb * 1024L)

    private val mem = UIntArray((bytes / 4).toInt()) // each element is 32-bit (4 bytes)
    val bytes = bytes

    /**
     * Reads an 8-bit value from the specified memory address.
     *
     * @param addr The memory address to read from.
     * @return The 8-bit value read from memory.
     */
    fun readByte(addr: Int): UByte {
        val index = addr / 4
        val shift = (addr % 4) * 8
        return ((mem[index].toInt() ushr shift) and 0xFF).toUByte()
    }

    /**
     * Writes an 8-bit value to the specified memory address.
     *
     * @param addr The memory address to write to.
     * @param value The 8-bit value to write.
     */
    fun writeByte(addr: Int, value: UByte) {
        val index = addr / 4
        val shift = (addr % 4) * 8
        val mask = (0xFFu shl shift).inv()
        mem[index] = (mem[index] and mask) or (value.toUInt() shl shift)
    }

    /**
     * Reads a 16-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 16-bit value read from memory.
     */
    fun readWord(addr: Int): UShort {
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
    fun writeWord(addr: Int, value: UShort) {
        writeByte(addr, (value and 0xFFu).toUByte())
        writeByte(addr + 1, ((value.toInt() ushr 8) and 0xFF).toUByte())
    }

    /**
     * Reads a 32-bit value from the specified memory address in little-endian format.
     *
     * @param addr The memory address to read from.
     * @return The 32-bit value read from memory.
     */
    fun readDWord(addr: Int): UInt = mem[addr / 4]

    /**
     * Writes a 32-bit value to the specified memory address in little-endian format.
     *
     * @param addr The memory address to write to.
     * @param value The 32-bit value to write.
     */
    fun writeDWord(addr: Int, value: UInt) {
        mem[addr / 4] = value
    }

    /**
     * Prints the memory content in a human-readable format.
     *
     * @param rows The number of rows to print. Defaults to the number of rows available in memory.
     */
    fun printMemory(rows: Int = ((bytes + 15) / 16).toInt().coerceAtLeast(1)) {
        println("Memory Dump:")
        println("Address    | " + (0 until 16).joinToString(" ") { "%02X".format(it) })
        println("-----------|-" + "-".repeat(47))
        val actualRows = rows.coerceAtMost(((bytes + 15) / 16).toInt().coerceAtLeast(1))
        for (i in 0 until actualRows) {
            val baseAddr = i * 16
            print("%08X   | ".format(baseAddr))
            for (j in 0 until 16) {
                val addr = baseAddr + j
                if (addr < bytes) print("%02X ".format(readByte(addr).toShort())) else print("   ")
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

    println("Byte at 00: %02X".format(memory.readByte(0x00).toShort()))
    println("Word at 02: %04X".format(memory.readWord(0x02).toShort()))
    println("DWord at 04: %08X".format(memory.readDWord(0x04).toShort()))
}