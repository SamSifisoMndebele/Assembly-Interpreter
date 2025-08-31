import asm.Interpreter
import asm.Parser
import cpu.Memory
import java.io.File
import java.io.FileNotFoundException
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    val sourceFile = if (args.isEmpty()) "src/main/kotlin/main.asm" else args[0]

    val src = try {
        File(sourceFile).readText()
    } catch (e: FileNotFoundException) {
        println("Error: Source file not found at '$sourceFile'")
        println("Please provide a valid path as a command-line argument or make sure the default file exists.")
        exitProcess(1)
    }

    try {
        val mem = Memory()
        val parser = Parser(src, mem)
        val parsed = parser.parseProgram()
        val interpreter = Interpreter(parsed.instructions, parsed.labels, mem)
        interpreter.run()
        interpreter.printRegisters()
    } catch (e: Exception) {
        // Catching a generic Exception is broad, but without knowing the specific
        // exceptions thrown by the parser or interpreter, it's a safe starting point.
        println("An error occurred during program execution: ${e.message}")
        e.printStackTrace()
        exitProcess(1)
    }
}