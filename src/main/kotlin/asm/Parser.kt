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
 * 1. **Symbol Table Construction:** Identifies labels and data symbols, storing their addresses.
 * 2. **Instruction Generation:** Parses instructions, resolving symbols to their addresses.
 *
 * **Memory Layout:**
 * - Data segment starts at `0x1000` (4KB) by default, leaving space for system/code.
 *
 * @property mem The [Memory] instance where data will be stored.
 * @param src The assembly source code as a string.
 */
class Parser(src: String, private val mem: Memory) {
    private val lex = Lexer(src)
    private var look: Token = lex.nextToken()
    private var currentSection = Section.CODE
    private val dataSymbols = mutableMapOf<String, Int>() // symbol -> address
    private var dataPtr = 0x1000 // Data segment starts at 4KB, leaving space for system/code.

    companion object {
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
     * Supports decimal, hexadecimal (0x prefix or h suffix).
     * The result is masked to ensure it fits within the target bit width.
     *
     * @param text The string representation of the number.
     * @return The parsed integer value.
     */
    private fun parseNumber(text: String): Int {
        val t = text.lowercase()
        return when {
            t.startsWith("0x") -> t.substring(2).toInt(16)
            t.endsWith("h") -> t.dropLast(1).toInt(16)
            else -> t.toInt()
        } // Masking is handled by memory write operations based on data type (DB, DW, DD)
    }

    /**
     * Parses a register name string (case-insensitive) into a [Reg] enum.
     *
     * @param id The string representation of the register (e.g., "AX", "al", "EBP").
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
        // 16-bit registers
        "AX"-> Reg.AX
        "BX"-> Reg.BX
        "CX"-> Reg.CX
        "DX"-> Reg.DX
        "SI"-> Reg.SI
        "DI"-> Reg.DI
        "BP"-> Reg.BP
        "SP"-> Reg.SP
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
                val reg = parseReg(idTok.text)
                if (reg != null) return Operand.RegOp(reg)
                val symAddr = dataSymbols[idTok.text]
                // `mov ax, var` -> `mov ax, [address_of_var]`
                if (symAddr != null) return Operand.MemOp(null, symAddr)
                // If it's not a register or a known data symbol, assume it's a code label.
                return Operand.LabelOp(idTok.text)
            }
            Token.Kind.NUMBER -> Operand.Imm32Op(parseNumber(eat(Token.Kind.NUMBER).text))
            Token.Kind.LBRACK -> {
                eat(Token.Kind.LBRACK)
                var base: Reg? = null
                var disp: Int? = null
                when (look.kind) {
                    Token.Kind.ID -> {
                        val idTok = eat(Token.Kind.ID)
                        val r = parseReg(idTok.text)
                        if (r != null) base = r else {
                            val addr = dataSymbols[idTok.text] ?: error("Unknown symbol ${idTok.text}")
                            disp = addr
                        }
                        if (tryEat(Token.Kind.PLUS) != null) disp = parseNumber(eat(Token.Kind.NUMBER).text)
                    }
                    Token.Kind.NUMBER -> { disp = parseNumber(eat(Token.Kind.NUMBER).text) }
                    else -> error("Line ${look.line}: bad memory operand")
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

        // Try direct match first
        opStringToEnumMap[upperId]?.let { return it }

        // If no direct match, check for suffixes B, W, D, L
        // Ensure the mnemonic isn't too short to be a base + suffix
        if (upperId.length >= 3) { // Smallest possible suffixed length (e.g., "ORB", "JGB")
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
        val labels = mutableMapOf<String, Int>()
        while (true) {
            when (look.kind) {
                Token.Kind.EOF -> return ParsedProgram(instructions, labels)
                Token.Kind.NEWLINE -> { eat(Token.Kind.NEWLINE); continue }
                Token.Kind.ID -> {
                    // Could be a label or an opcode
                    val idTok = eat(Token.Kind.ID)
                    val id = idTok.text

                    // Comments
                    if (tryEat(Token.Kind.COLON) != null) {
                        labels[id] = instructions.size // points to the next instruction index
                        // consume any trailing tokens until the newline
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    // Check section
                    if (id.startsWith(".")) {
                        when(id.lowercase()) {
                            ".data" -> currentSection = Section.DATA
                            ".code" -> currentSection = Section.CODE
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    // Opcode
                    if (currentSection == Section.CODE) {
                        val op = parseOp(id)
                        var dst: Operand? = null
                        var src: Operand? = null
                        if (!isNewlineOrEOF()) {
                            // dst
                            dst = parseOperand()
                            if (tryEat(Token.Kind.COMMA) != null) src = parseOperand()
                            // eat rest of line
                            while (!isNewlineOrEOF()) look = lex.nextToken()
                        }
                        tryEat(Token.Kind.NEWLINE)
                        instructions.add(Instruction(op, dst, src, idTok.line))
                        continue
                    }

                    // Data
                    if (currentSection == Section.DATA) {
                        val typeTok = eat(Token.Kind.ID)
                        val valTok = if (look.kind == Token.Kind.NUMBER) eat(Token.Kind.NUMBER) else null
                        val value = if (valTok != null) parseNumber(valTok.text) else 0
                        dataSymbols[id] = dataPtr

                        when (typeTok.text.uppercase()) {
                            "DB", "BYTE" -> { // Define Byte (8-bit)
                                if (value < -128 || value > 255) error("Line ${typeTok.line}: Value out of 8-bit range for DB/BYTE: $value")
                                mem.write8(dataPtr.toLong(), value.toLong())
                                dataPtr += 1
                            }
                            "DW", "WORD" -> { // Define Word (16-bit)
                                if (value < -32768 || value > 65535) error("Line ${typeTok.line}: Value out of 16-bit range for DW/WORD: $value")
                                mem.write16(dataPtr.toLong(), value.toLong())
                                dataPtr += 2
                            }
                            "DD", "DWORD", "LONG" -> { // Define Double Word (32-bit)
                                // No practical upper limit for Int in Kotlin for positive, but consider signed range
                                // mem.write32 will handle the conversion to bytes
                                mem.write32(dataPtr.toLong(), value.toLong())
                                dataPtr += 4
                            }
                            else -> error("Line ${typeTok.line}: Unsupported data directive '${typeTok.text}'. Supported: DB, BYTE, DW, WORD, DD, DWORD, LONG")
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }
                }
                else -> {
                    // skip unexpected till newline
                    while (!isNewlineOrEOF()) look = lex.nextToken()
                    tryEat(Token.Kind.NEWLINE)
                }
            }
        }
    }
}
