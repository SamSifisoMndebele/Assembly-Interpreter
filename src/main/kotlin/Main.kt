import asm.Interpreter
import asm.Parser
import java.io.File

fun main(args: Array<String>) {
    val sourceFile = if (args.isEmpty()) "src/main/kotlin/main.asm" else args[0]
    val src = File(sourceFile).readText()
    val parser = Parser(src)
    val parsed = parser.parseProgram()
    val interpreter = Interpreter(parsed.instructions, parsed.labels)
    interpreter.run()
    interpreter.printRegisters()
}