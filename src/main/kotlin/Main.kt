import cpu.Memory
import parsing.Parser
import java.io.File
import java.io.FileNotFoundException
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    val sourceFile = if (args.isEmpty()) "src/main/kotlin/main.asm" else args[0]

    val src = try {
        File(sourceFile).readText()
    } catch (e: FileNotFoundException) {
        println("Error: ${e.message}, Source file not found at '$sourceFile'")
        println("Please provide a valid path as a command-line argument or make sure the default file exists.")
        exitProcess(1)
    }

    try {
        val mem = Memory(8)
        val parser = Parser(src, mem)
//        val parsed = parser.parseProgram()
//        val cpu = CPU(mem, parsed.labels, 2)
//        cpu.run(parsed.instructions)

//        cpu.printRegisters()
        mem.dumpMemory()
    } catch (e: Exception) {
        println("An error occurred during program execution: ${e.message}")
        e.printStackTrace()
        exitProcess(1)
    }
}