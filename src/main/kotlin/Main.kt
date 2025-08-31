import java.io.File

fun main(args: Array<String>) {
    val src = if (args.isEmpty()) SAMPLE else File(args[0]).readText()
    println("--- Mini MASM Interpreter (MVP) ---")
    println("Source length: ${src.length} chars")
    val parser = Parser(src)
    val parsed = parser.parseProgram()
    val interpreter = Interpreter(parsed.instructions, parsed.labels)
    interpreter.run()
    println("--- Done ---")
}

private val SAMPLE = """
        ; Demo: compute 5 + 7, store to [1000], then simple loop
        MOV AX, 5
        ADD AX, 7
        MOV [1000], AX

        ; countdown from 3 to 0
        MOV CX, 3
    loop_start:
        DEC CX
        CMP CX, 0
        JG loop_start ; jump if greater than 0

        INT 20h ; terminate
    """.trimIndent()