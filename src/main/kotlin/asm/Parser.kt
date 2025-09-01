package asm

import cpu.Memory
import model.*

/**
 * Parses assembly source code into a list of instructions and a map of labels.
 *
 * The parser supports the following features:
 * - Sections: `.data` and `.code` for organizing data and instructions.
 * - Labels: For defining jump targets and data locations.
 * - Instructions: A subset of x86 assembly instructions (MOV, ADD, SUB, etc.).
 * - Operands: Registers (AX, BX, etc.), immediate values, memory addresses (direct and register-based).
 * - Data Directives: `DB` (Define Byte), `DW` (Define Word), `DD` (Define Double Word) for reserving and initializing memory.
 * - Comments: Lines starting with `;` or everything after a `:` in a label definition.
 *
 * The parser performs a two-pass process (conceptually):
 * 1. **Symbol Table Construction:** Identifies labels and data symbols, storing their addresses/offsets.
 * 2. **Instruction Generation:** Parses instructions, resolving symbols to their addresses/offsets.
 *
 * **Memory Layout:**
 * - Data segment starts at a defined base address (e.g., `0x1000`).
 *
 * @property mem The [Memory] instance where data will be stored.
 * @param src The assembly source code as a string.
 */
class Parser(src: String, private val mem: Memory) {
    private val lex = Lexer(src)
    private var look: Token = lex.nextToken()
    private var currentSection = Section.CODE
    private val dataSymbols = mutableMapOf<String, Long>() // symbol -> offset within data segment
    private var currentDataOffset = 0L // Current offset within the data segment
    
    companion object {
        private const val DATA_SEGMENT_BASE = 0x1000 // Data segment starts at 4KB

        private val opStringToEnumMap: Map<String, Op> = mapOf(
            "MOV" to Op.MOV, "ADD" to Op.ADD, "SUB" to Op.SUB, "INC" to Op.INC, "DEC" to Op.DEC,
            "CMP" to Op.CMP, "JMP" to Op.JMP, "JE" to Op.JE, "JNE" to Op.JNE, "JG" to Op.JG,
            "JL" to Op.JL, "JGE" to Op.JGE, "JLE" to Op.JLE, "PUSH" to Op.PUSH, "POP" to Op.POP,
            "CALL" to Op.CALL, "RET" to Op.RET, "INT" to Op.INT, "NOP" to Op.NOP,
            "XCHG" to Op.XCHG, "LEA" to Op.LEA, "MUL" to Op.MUL, "IMUL" to Op.IMUL,
            "DIV" to Op.DIV, "IDIV" to Op.IDIV, "NEG" to Op.NEG, "AND" to Op.AND,
            "OR" to Op.OR, "XOR" to Op.XOR, "NOT" to Op.NOT, "TEST" to Op.TEST,
            "SHL" to Op.SHL, "SAL" to Op.SAL,
            "SHR" to Op.SHR, "SAR" to Op.SAR,
            "ROL" to Op.ROL, "ROR" to Op.ROR, "RCL" to Op.RCL, "RCR" to Op.RCR,
            "MOVS" to Op.MOVS, "CMPS" to Op.CMPS, "SCAS" to Op.SCAS, "LODS" to Op.LODS,
            "STOS" to Op.STOS
        )
    }

    /**
     * Consumes the current token if its kind matches the expected kind.
     *
     * @param kind The expected [Token.Kind].
     * @return The consumed [Token].
     * @throws IllegalStateException if the current token's kind does not match the expected kind.
     */
    private fun eat(kind: Token.Kind): Token {
        if (look.kind != kind) error("Parse error at line ${look.line}: expected $kind, got ${look.kind}")
        val t = look
        look = lex.nextToken()
        return t
    }

    /**
     * Tries to consume the current token if its kind matches the expected kind.
     *
     * @param kind The expected [Token.Kind].
     * @return The consumed [Token] if successful, or `null` otherwise.
     */
    private fun tryEat(kind: Token.Kind): Token? = if (look.kind == kind) eat(kind) else null

    /**
     * Checks if the current token is a newline or end-of-file.
     *
     * @return `true` if the current token is a newline or EOF, `false` otherwise.
     */
    private fun isNewlineOrEOF() = look.kind == Token.Kind.NEWLINE || look.kind == Token.Kind.EOF

    /**
     * Parses a string representation of a number into an integer.
     * Supports decimal, hexadecimal (0x prefix or h suffix),
     * binary (0b prefix or b suffix), and octal (0o prefix or o/q suffix).
     *
     * @throws NumberFormatException if the string is not a valid representation of a number in any supported base.
     * @param text The string representation of the number.
     * @return The parsed integer value.
     */
    private fun parseImmediate(text: String): Long {
        val t = text.lowercase().replace("_", "") // Allow underscores for readability
        return when {
            // Hexadecimal
            t.startsWith("0x") -> t.substring(2).toLong(16)
            t.endsWith("h") -> t.dropLast(1).toLong(16)
            // Binary
            t.startsWith("0b") -> t.substring(2).toLong(2)
            t.endsWith("b") -> t.dropLast(1).toLong(2)
            // Octal
            t.startsWith("0o") -> t.substring(2).toLong(8)
            t.endsWith("o") || t.endsWith("q") -> t.dropLast(1).toLong(8)
            // Decimal (default)
            t.all { it.isDigit() || (it == '-' && t.indexOf('-') == 0) } -> t.toLong()
            // Handle character literals like 'A'
            t.length == 3 && t.startsWith("'") && t.endsWith("'") -> t[1].code.toLong()
            else -> t.toLong()
        }
    }

    /**
     * Parses a register name string (case-insensitive) into a [Reg] enum.
     *
     * @param id The string representation of the register (e.g., "AX", "al", "EBP", "DS").
     * @return The corresponding [Reg] enum if the string is a valid register name, or `null` otherwise.
     */
    private fun parseReg(id: String): Reg? = when(id.uppercase()) {
        // 8-bit registers
        "AL" -> Reg.AL
        "AH" -> Reg.AH
        "BL" -> Reg.BL
        "BH" -> Reg.BH
        "CL" -> Reg.CL
        "CH" -> Reg.CH
        "DL" -> Reg.DL
        "DH" -> Reg.DH
        // 16-bit general-purpose registers
        "AX"-> Reg.AX
        "BX"-> Reg.BX
        "CX"-> Reg.CX
        "DX"-> Reg.DX
        "SI"-> Reg.SI
        "DI"-> Reg.DI
        "BP"-> Reg.BP
        "SP"-> Reg.SP
        // 16-bit segment registers
        "CS" -> Reg.CS
        "DS" -> Reg.DS
        "SS" -> Reg.SS
        "ES" -> Reg.ES
        // 32-bit registers
        "EAX" -> Reg.EAX
        "EBX" -> Reg.EBX
        "ECX" -> Reg.ECX
        "EDX" -> Reg.EDX
        "ESI" -> Reg.ESI
        "EDI" -> Reg.EDI
        "EBP" -> Reg.EBP
        "ESP" -> Reg.ESP
        else -> null
    }

    /**
     * Parses an operand from the token stream.
     *
     * @return The parsed [Operand].
     */
    private fun parseOperand(): Operand {
        return when (look.kind) {
            Token.Kind.ID -> {
                val idTok = eat(Token.Kind.ID)
                parseReg(idTok.text)?.let { return Operand.RegOp(it) }
                dataSymbols[idTok.text]?.let { offset -> 
                    // When a symbol is used as an operand like `mov ax, myVar`,
                    // it implies `mov ax, [offset_of_myVar_in_data_segment]`.
                    // The interpreter will need to add DATA_SEGMENT_BASE (or DS register value)
                    // to this offset.
                    return Operand.MemOp(null, offset) 
                }
                // If it's not a register or a known data symbol, assume it's a code label.
                Operand.LabelOp(idTok.text)
            }
            Token.Kind.NUMBER -> Operand.ImmOp(parseImmediate(eat(Token.Kind.NUMBER).text))
            Token.Kind.LBRACK -> {
                eat(Token.Kind.LBRACK)
                var base: Reg? = null
                var disp: Long? = null
                when (look.kind) {
                    Token.Kind.ID -> {
                        val idTok = eat(Token.Kind.ID)
                        val r = parseReg(idTok.text)
                        if (r != null) {
                            base = r
                            // Check for displacement after register e.g. [bx+4]
                            if (tryEat(Token.Kind.PLUS) != null) {
                                disp = parseImmediate(eat(Token.Kind.NUMBER).text)
                            }
                        } else {
                            // Assumed to be a data symbol e.g. [myVar] or [myVar+4]
                            val symbolOffset = dataSymbols[idTok.text] 
                                ?: error("Line ${idTok.line}: Unknown symbol '${idTok.text}' in memory operand")
                            disp = symbolOffset
                            if (tryEat(Token.Kind.PLUS) != null) {
                                val immediateOffset = parseImmediate(eat(Token.Kind.NUMBER).text)
                                disp = disp + immediateOffset
                            }
                        }
                    }
                    Token.Kind.NUMBER -> { 
                        disp = parseImmediate(eat(Token.Kind.NUMBER).text)
                        // Optional: Could support [4+bx] here if needed
                    }
                    else -> error("Line ${look.line}: bad memory operand, expected ID or NUMBER inside brackets")
                }
                eat(Token.Kind.RBRACK)
                Operand.MemOp(base, disp)
            }
            else -> error("Line ${look.line}: unexpected token ${look.kind}")
        }
    }

    /**
     * Parses an opcode string into an [Op] enum.
     *
     * @param id The string representation of the opcode.
     * @return The corresponding [Op].
     */
    private fun parseOp(id: String): Op {
        val upperId = id.uppercase()

        opStringToEnumMap[upperId]?.let { return it }

        if (upperId.length >= 2) { // Allow single char suffix like "MOVB"
            val lastChar = upperId.last()
            if (lastChar == 'B' || lastChar == 'W' || lastChar == 'D' || lastChar == 'L') {
                val baseMnemonic = upperId.dropLast(1)
                opStringToEnumMap[baseMnemonic]?.let { return it }
            }
        }
        error("Line ${look.line}: unknown opcode '$id' (parsed as '$upperId')")
    }

    /**
     * Data class to hold the result of parsing the program.
     *
     * @property instructions The list of parsed [Instruction]s.
     * @property labels A mutable map of label names to their corresponding instruction index.
     */
    data class ParsedProgram(
        val instructions: List<Instruction>,
        val labels: MutableMap<String, Int> // label -> instruction index
    )

    /**
     * Parses the entire assembly program.
     * @return A [ParsedProgram] object containing the parsed instructions and labels.
     */
    fun parseProgram(): ParsedProgram {
        val instructions = mutableListOf<Instruction>()
        val labels = mutableMapOf<String, Int>() // label name -> instruction index for code labels
        
        while (true) {
            when (look.kind) {
                Token.Kind.EOF -> return ParsedProgram(instructions, labels)
                Token.Kind.NEWLINE -> { eat(Token.Kind.NEWLINE); continue }
                Token.Kind.ID -> {
                    val idTok = eat(Token.Kind.ID)
                    val id = idTok.text

                    if (tryEat(Token.Kind.COLON) != null) { // Label definition
                        if (currentSection == Section.CODE) {
                            labels[id] = instructions.size // Code label points to the next instruction index
                        } else if (currentSection == Section.DATA) {
                            // Labels in .data section point to current data offset
                            dataSymbols[id] = currentDataOffset 
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken() // Consume rest of line
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    if (id.startsWith(".")) { // Section directive
                        when(id.lowercase()) {
                            ".data" -> {
                                currentSection = Section.DATA
                                currentDataOffset = 0 // Reset offset when switching to data section
                            }
                            ".code" -> currentSection = Section.CODE
                            // TODO: Add other sections like .stack, .bss if needed
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    // Opcode or Data definition
                    if (currentSection == Section.CODE) {
                        val op = parseOp(id)
                        var dst: Operand? = null
                        var src: Operand? = null
                        if (!isNewlineOrEOF()) {
                            dst = parseOperand()
                            if (tryEat(Token.Kind.COMMA) != null) src = parseOperand()
                            while (!isNewlineOrEOF()) look = lex.nextToken()
                        }
                        tryEat(Token.Kind.NEWLINE)
                        instructions.add(Instruction(op, dst, src, idTok.line))
                    } else if (currentSection == Section.DATA) {
                        // id is the variable name, look is now type (DB, DW, etc.)
                        val symbolName = id 
                        val typeTok = eat(Token.Kind.ID) // DB, DW, etc.
                        val valTok = if (look.kind == Token.Kind.NUMBER) eat(Token.Kind.NUMBER) else null
                        val value = if (valTok != null) parseImmediate(valTok.text) else 0
                        
                        dataSymbols[symbolName] = currentDataOffset // Store offset before incrementing

                        val physicalAddress = (DATA_SEGMENT_BASE + currentDataOffset).toLong()

                        when (typeTok.text.uppercase()) {
                            "DB", "BYTE" -> {
                                if (value < -128 || value > 255) error("Line ${typeTok.line}: Value out of 8-bit range for DB/BYTE: $value")
                                mem.write8(physicalAddress, value.toLong())
                                currentDataOffset += 1
                            }
                            "DW", "WORD" -> {
                                if (value < -32768 || value > 65535) error("Line ${typeTok.line}: Value out of 16-bit range for DW/WORD: $value")
                                mem.write16(physicalAddress, value.toLong())
                                currentDataOffset += 2
                            }
                            "DD", "DWORD", "LONG" -> {
                                mem.write32(physicalAddress, value.toLong())
                                currentDataOffset += 4
                            }
                            else -> error("Line ${typeTok.line}: Unsupported data directive '${typeTok.text}'. Supported: DB, BYTE, DW, WORD, DD, DWORD, LONG")
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                    }
                }
                else -> { // Skip unexpected tokens until newline
                    while (!isNewlineOrEOF()) look = lex.nextToken()
                    tryEat(Token.Kind.NEWLINE)
                }
            }
        }
    }
}
