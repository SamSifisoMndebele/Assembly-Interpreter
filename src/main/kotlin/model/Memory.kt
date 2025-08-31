package model

class Memory(memSize: Int = 64) { // 64KB
    private val mem = ByteArray(memSize * 1024)
    val size = mem.size

    fun read8(addr: Int): Int = mem[addr and 0xFFFF].toInt() and 0xFF
    fun write8(addr: Int, value: Int) {
        mem[addr and 0xFFFF] = (value and 0xFF).toByte()
    }

    fun read16(addr: Int): Int {
        val lo = read8(addr)
        val hi = read8(addr + 1)
        return (hi shl 8) or lo
    }
    fun write16(addr: Int, value: Int) {
        write8(addr, value and 0xFF)
        write8(addr + 1, (value ushr 8) and 0xFF)
    }
}