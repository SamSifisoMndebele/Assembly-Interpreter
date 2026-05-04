import assemble.Symbol
import machine.Memory
import machine.VirtualCPU
import parsing.Parser
import java.io.File
import java.io.FileNotFoundException
import kotlin.math.max
import kotlin.math.min
import kotlin.system.exitProcess

fun main() {
    val src = try {
        File("src/main/kotlin/main.asm").readText()
    } catch (e: FileNotFoundException) {
        println("Error: ${e.message}, Source file not found: src/main/kotlin/main.asm")
        println("Please provide a valid path as a command-line argument or make sure the default file exists.")
        exitProcess(1)
    }

    val memory = Memory(1024L) // Example: 1KB of memory
    val parser = Parser(src, memory)
    val cpu = VirtualCPU(memory, startingAddress = parser.codeSegmentBase)

    cpu.run()
    cpu.dumpRegisters()

    // --- Symbol table ---
//    dumpSymbolTable(parser.getSymbols())
    // --- Memory dump ---
//    dumpMemorySegments(memory, parser)
    // --- Full memory dump ---
//    memory.dumpMemory()
}

fun dumpMemorySegments(memory: Memory, parser: Parser) {
    val dataSegStr = parser.dataSegmentBase.toString(16)
    val codeSegStr = parser.codeSegmentBase.toString(16)
    val stackSegStr = parser.stackSegmentBase.toString(16)

    println("\n--- Main function reporting --- ")
    println("Parser instance created. Check console output for parsing details and memory layout.")
    println("Code Segment Base (from parser): ${codeSegStr}h")
    println("Data Segment Base (from parser): ${dataSegStr}h")
    println("Stack Segment Base (from parser): ${stackSegStr}h")

    if (parser.dataSegmentBase < memory.bytes) {
        println("\nMemory content of Data Segment start (${dataSegStr}h):")
        val startAddr = parser.dataSegmentBase
        val endAddr = min(startAddr + 128, memory.bytes) // Print up to 128 bytes or end of memory
        memory.dumpMemory(start = startAddr, end = endAddr)
    }

    if (parser.codeSegmentBase < memory.bytes) {
        println("\nMemory content of Code Segment start (${codeSegStr}h):")
        val startAddr = parser.codeSegmentBase
        val endAddr = min(startAddr + 128, memory.bytes) // Print up to 128 bytes or end of memory
        memory.dumpMemory(start = startAddr, end = endAddr)
    }

    println("\nMemory content of Stack Segment start (${stackSegStr}h):")
    val endAddr = parser.stackSegmentBase
    val startAddr = max(endAddr - 128, 0) // Print up to 128 bytes or end of memory
    memory.dumpMemory(start = startAddr, end = endAddr)
}

fun dumpSymbolTable(symbols: Map<String, Symbol>) {
    println("+------------------+----------+")
    println("| Name             | Address  |")
    println("+------------------+----------+")
    symbols.forEach { (name, address) ->
        println(String.format("| %-16s | %-8X |", name, address))
    }
    println("+------------------+----------+")
}
