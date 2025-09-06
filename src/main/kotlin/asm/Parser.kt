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
 * - Data segment starts at a defined base address (e.g., `0x1000u`).
 *
 * @property mem The [Memory] instance where data will be stored.
 * @param src The assembly source code as a string.
 */
class Parser(src: String, private val mem: Memory) {
    private val lex = Lexer(src)
    private var look: Token = lex.nextToken()
    private var currentSection = Section.CODE
    private val symbolTable = mutableMapOf<String, UInt>() // symbol -> offset within data segment
    private var dataOffset = 0L // Current offset within the data segment
    
    companion object {
        private val String.asOperation: Operation?
            get() =  when (this.uppercase()) {
                "MOV" -> Operation.OperationTwo.MOV
                "ADD" -> Operation.OperationTwo.ADD
                "SUB" -> Operation.OperationTwo.SUB
                "XCHG" -> Operation.OperationTwo.XCHG
                "INC" -> Operation.OperationOne.INC
                "DEC" -> Operation.OperationOne.DEC
                "PUSH" -> Operation.OperationOne.PUSH
                "POP" -> Operation.OperationOne.POP
                "JMP" -> Operation.OperationOne.JMP
                "CALL" -> Operation.OperationOne.CALL
                "RET" -> Operation.OperationZero.RET
                "NOP" -> Operation.OperationZero.NOP
                "CMP" -> Operation.OperationTwo.CMP
                "JG", "JNLE" -> Operation.OperationOne.JG
                "AND" -> Operation.OperationTwo.AND
                "OR" -> Operation.OperationTwo.OR
                "XOR" -> Operation.OperationTwo.XOR
//                "TEST" -> Operation.OperationTwo.TEST
//                "LEA" -> Operation.OperationTwo.LEA
                "NEG" -> Operation.OperationOne.NEG
                "NOT" -> Operation.OperationOne.NOT
                "MUL" -> Operation.OperationOne.MUL
                "DIV" -> Operation.OperationOne.DIV
//                "IDIV" -> Operation.OperationOne.IDIV
                "JE", "JZ" -> Operation.OperationOne.JE
                "JNE", "JNZ" -> Operation.OperationOne.JNE
                "JL", "JNGE" -> Operation.OperationOne.JL
                "JGE", "JNL" -> Operation.OperationOne.JGE
                "JLE", "JNG" -> Operation.OperationOne.JLE
//                "JA", "JNBE" -> Operation.OperationOne.JA
                "JAE", "JNB", "JNC" -> Operation.OperationOne.JAE
                "JB", "JNAE", "JC" -> Operation.OperationOne.JB
//                "JBE", "JNA" -> Operation.OperationOne.JBE
//                "INT" -> Operation.OperationOne.INT

                // Shift and Rotate operations - Assuming they are two-operand for now
//                 "SHL", "SAL" -> Operation.OperationTwo.SHL
//                 "SHR" -> Operation.OperationTwo.SHR
//                 "SAR" -> Operation.OperationTwo.SAR
//                 "ROL" -> Operation.OperationTwo.ROL
//                 "ROR" -> Operation.OperationTwo.ROR
//                 "RCL" -> Operation.OperationTwo.RCL
//                 "RCR" -> Operation.OperationTwo.RCR
                else -> null
            }
    }

    /**
     * Consumes the current token if it matches the expected kind.
     *
     * Advances the lexer to the next token.
     *
     * @param kind The expected [Token.Kind].
     * @return The consumed [Token].
     * @throws IllegalStateException if the current token does not match the expected kind.
     */
    private fun eat(kind: Token.Kind): Token {
        if (look.kind != kind) error("Parse error at line ${look.line}: expected $kind, got ${look.kind} ('${look.text}')")
        val t = look
        look = lex.nextToken()
        return t
    }

    /**
     * Attempts to consume the current token if it matches the expected [kind].
     * If the current token's kind matches, it calls [eat] to consume it and advance
     * to the next token, returning the consumed token.
     * If the kind does not match, it returns `null` and the parser state remains unchanged.
     *
     * @param kind The [Token.Kind] to attempt to consume.
     * @return The consumed [Token] if the kind matched, or `null` otherwise.
     */
    private fun tryEat(kind: Token.Kind): Token? = if (look.kind == kind) eat(kind) else null

    private fun isNewlineOrEOF() = look.kind == Token.Kind.NEWLINE || look.kind == Token.Kind.EOF

    private fun parseEscapedChar(char: Char): Char {
        return when (char) {
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            '\\' -> '\\'
            '\'' -> '\''
            '"' -> '"'
            '0' -> '\u0000' // Null character
            // Add more escaped characters as needed
            else -> error("Unsupported escape sequence: \\$char")
        }
    }

    /**
     * Parses an immediate value string into an unsigned integer.
     * Supports various number formats:
     * - Hexadecimal (ends with 'h', e.g., "0FFh")
     * - Binary (ends with 'b', e.g., "1010b")
     * - Octal (ends with 'q', e.g., "77q")
     * - Decimal (ends with 'd' or no suffix, e.g., "123d", "255")
     * Underscores are ignored for readability (e.g., "1_000_000" is treated as "1000000").
     * Character literals (e.g., 'A') are also handled as their ASCII/Unicode values if tokenized as ID or NUMBER.
     *
     * @param text The string representation of the immediate value.
     * @return The parsed [UInt] value.
     * @throws NumberFormatException if the string cannot be parsed into a valid number in any of the supported formats.
     */
    private fun parseImmediate(text: String): UInt {
        println("Parsing immediate: $text")
        val t = text.lowercase().replace("_", "")
        return when {
            t.endsWith("h") -> t.dropLast(1).toUInt(16)
            t.endsWith("b") -> t.dropLast(1).toUInt(2)
            t.endsWith("q") -> t.dropLast(1).toUInt(8)
            t.endsWith("d") -> t.dropLast(1).toUInt()
            else -> try {
                t.toUInt()
            } catch (_: NumberFormatException) {
                throw NumberFormatException("Invalid number format: $text -> $t")
            }
        }
    }
    
    /**
     * Parses a string literal, handling escape sequences.
     *
     * Converts a string (potentially with escape sequences like `\n`, `\t`, `\\`)
     * into an array of UInts, where each UInt represents the character code
     * of the corresponding character in the parsed string.
     *
     * For example:
     * - `"Hello"` becomes `[72u, 101u, 108u, 108u, 111u]`
     * - `"A\nB"` becomes `[65u, 10u, 66u]` (where 10u is the code for newline)
     * - `"\\"` becomes `[92u]` (where 92u is the code for backslash)
     *
     * @param text The raw string literal to parse (e.g., the content between quotes,
     *             excluding the quotes themselves).
     * @return An array of UInts representing the character codes of the parsed string.
     * @throws error if an unsupported escape sequence is encountered.
     * @see parseEscapedChar
     */
    private fun parseString(text: String): Array<UInt> {
        val result = mutableListOf<UInt>()
        var i = 0
        while (i < text.length) {
            val char = text[i]
            if (char == '\\' && i + 1 < text.length) {
                result.add(parseEscapedChar(text[i+1]).code.toUInt())
                i += 2
            } else {
                result.add(char.code.toUInt())
                i += 1
            }
        }
        return result.toTypedArray()
    }

    /**
     * Parses a string identifier into a [Reg] enumeration value.
     * This function is case-insensitive.
     *
     * @param id The string representation of the register (e.g., "AX", "al", "EBP").
     * @return The corresponding [Reg] enum value if the identifier is a valid register name,
     *         otherwise `null`.
     */
    private fun parseReg(id: String): Reg? = when(id.uppercase()) {
        "AL" -> Reg.AL
        "AH" -> Reg.AH
        "BL" -> Reg.BL
        "BH" -> Reg.BH
        "CL" -> Reg.CL
        "CH" -> Reg.CH
        "DL" -> Reg.DL
        "DH" -> Reg.DH
        "AX"-> Reg.AX
        "BX"-> Reg.BX
        "CX"-> Reg.CX
        "DX"-> Reg.DX
        "SI"-> Reg.SI
        "DI"-> Reg.DI
        "BP"-> Reg.BP
        "SP"-> Reg.SP
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
     * Parses an operand from the current token stream.
     *
     * An operand can be:
     * - **Register:** (e.g., `AX`, `BL`)
     * - **Immediate Value:** A numeric literal (e.g., `123`, `0FFh`).
     * - **Memory Address:**
     *     - Direct: A symbol defined in the `.data` section (e.g., `myVar`).
     *     - Register Indirect: `[BX]`
     *     - Register Indirect with Displacement: `[BX+5]` or `[mySymbol+5]` or `[5]` (which is `[0+5]`)
     * - **Label:** An identifier that is not a register, known data symbol, or valid immediate,
     *   assumed to be a code label for jumps/calls.
     *
     * @return The parsed [Operand].
     * @throws IllegalStateException if an unexpected token is encountered or if syntax is incorrect.
     */
    private fun parseOperand(): Operand {
        return when (look.kind) {
            Token.Kind.ID -> {
                val idTok = eat(Token.Kind.ID)
                val tokenText = idTok.text
                parseReg(tokenText)?.let { return Operand.Register(it) }
                symbolTable[tokenText]?.let { offset -> // offset is UInt
                    return Operand.Memory(null, offset) // MemOp expects UInt?
                }
                return try {
                    Operand.Immediate(parseImmediate(tokenText))
                } catch (_: NumberFormatException) {
                    Operand.Label(tokenText)
                }
            }
            Token.Kind.NUMBER -> Operand.Immediate(parseImmediate(eat(Token.Kind.NUMBER).text)) // parseImmediate returns UInt
            Token.Kind.STRING -> { // Added to handle strings as direct operands if necessary, though data section is primary
                val strTok = eat(Token.Kind.STRING)
                // This typically would be an error for most instruction operands or needs specific handling.
                // For now, let's treat it as a label, which might not be what's intended for general operands.
                // Or, if it's a character, parseImmediate would handle it if tokenized as ID or NUMBER.
                error("Line ${strTok.line}: String literal \"${strTok.text}\" not directly usable as instruction operand here. Define in .data section.")
            }
            Token.Kind.LBRACK -> {
                eat(Token.Kind.LBRACK)
                var base: Reg? = null
                var disp: UInt? = null // Changed to UInt?
                when (look.kind) {
                    Token.Kind.ID -> {
                        val idTok = eat(Token.Kind.ID)
                        val r = parseReg(idTok.text)
                        if (r != null) {
                            base = r
                            if (tryEat(Token.Kind.PLUS) != null) {
                                if (look.kind == Token.Kind.NUMBER) {
                                   disp = parseImmediate(eat(Token.Kind.NUMBER).text) // returns UInt
                                } else {
                                    error("Line ${look.line}: Expected number after '+' in memory operand")
                                }
                            }
                        } else {
                            val symbolOffset = symbolTable[idTok.text] // UInt?
                                ?: error("Line ${idTok.line}: Unknown symbol '${idTok.text}' in memory operand")
                            disp = symbolOffset
                            if (tryEat(Token.Kind.PLUS) != null) {
                                if (look.kind == Token.Kind.NUMBER) {
                                    val immediateOffset = parseImmediate(eat(Token.Kind.NUMBER).text) // UInt
                                    disp = disp + immediateOffset // disp becomes UInt
                                } else {
                                     error("Line ${look.line}: Expected number after '+' in memory operand with symbol")
                                }
                            }
                        }
                    }
                    Token.Kind.NUMBER -> { 
                        disp = parseImmediate(eat(Token.Kind.NUMBER).text) // returns UInt
                    }
                    else -> error("Line ${look.line}: bad memory operand, expected ID or NUMBER inside brackets")
                }
                eat(Token.Kind.RBRACK)
                Operand.Memory(base, disp) // disp is UInt?
            }
            else -> error("Line ${look.line}: unexpected token ${look.kind}")
        }
    }

    /**
     * Parses an opcode string and returns the corresponding [Operation] object.
     *
     * This function converts the input `id` (opcode string) to uppercase and attempts
     * to directly look it up in a predefined mapping of opcode strings to [Operation] objects
     * (via the `asOperation` extension property).
     *
     * Previously, this function might have handled suffixes like 'B', 'W', 'D' to determine
     * operation size. However, this logic has been removed. Operations with different
     * sizes (e.g., MOVB, MOVW, MOVD) are now expected to be distinct entries in the
     * opcode-to-operation map, each mapping to a specific [Operation] object
     * (e.g., `Operation.OperationTwo.MOVB` if such an operation is defined).
     *
     * If the opcode string (after being uppercased) is not found in the mapping,
     * this function throws an error indicating an unknown opcode, including the
     * line number from the `look` token and the original and parsed opcode strings.
     *
     * @param id The opcode string to parse (e.g., "MOV", "add", "JMP").
     * @return The [Operation] object corresponding to the given opcode string.
     * @throws IllegalStateException if the opcode string is unknown.
     */// Updated to return Operation
    private fun parseOperation(id: String): Operation {
        val upperId = id.uppercase()
        // Direct lookup first
        upperId.asOperation?.let { return it }

        // Removed suffix logic as Operation objects should be distinct
        // If MOVB, MOVW, MOVD are needed, they should be distinct entries in opStringToOperationMap
        // mapping to specific Operation objects e.g. Operation.OperationTwo.MOVB if that exists.

        error("Line ${look.line}: unknown opcode '$id' (parsed as '$upperId')")
    }

    data class ParsedProgram(
        val instructions: List<Instruction>,
        val labels: MutableMap<String, UInt> // label -> instruction index (UInt)
    )

    /**
     * Parses the entire assembly program.
     * ... (rest of kdoc)
     */
    fun parseProgram(): ParsedProgram {
        val instructions = mutableListOf<Instruction>()
        val labels = mutableMapOf<String, UInt>() 
        
        while (true) {
            when (look.kind) {
                Token.Kind.EOF -> return ParsedProgram(instructions, labels)
                Token.Kind.NEWLINE -> { eat(Token.Kind.NEWLINE); continue }
                Token.Kind.ID -> {
                    val idTok = eat(Token.Kind.ID)
                    val id = idTok.text

                    if (tryEat(Token.Kind.COLON) != null) { // Label definition
                        if (currentSection == Section.CODE) {
                            labels[id] = instructions.size.toUInt()
                        } else if (currentSection == Section.DATA) {
                            symbolTable[id] = dataOffset.toUInt()
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken() // Consume rest of line (e.g. comment)
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    if (id.startsWith(".")) { // Section directive
                        when(id.lowercase()) {
                            ".data" -> {
                                currentSection = Section.DATA
                                dataOffset = 0 // Reset data offset for each .data section (or manage globally)
                            }
                            ".code" -> currentSection = Section.CODE
                            // Add other directives like .text if needed
                            else -> error("Line ${idTok.line}: Unknown directive '$id'")
                        }
                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    }

                    if (currentSection == Section.CODE) {
                        val op = parseOperation(id)
                        var dst: Operand? = null
                        var src: Operand? = null
                        
                        if (!isNewlineOrEOF() && look.kind != Token.Kind.COLON /* for comments after instruction */) {
                            dst = parseOperand()
                            if (tryEat(Token.Kind.COMMA) != null) {
                                if (!isNewlineOrEOF() && look.kind != Token.Kind.COLON) {
                                    src = parseOperand()
                                } else {
                                     error("Line ${idTok.line}: Expected second operand after comma for instruction '$id'")
                                }
                            }
                        }
                        
                        // Consume any EOL comments or just the EOL
                        while (!isNewlineOrEOF()) look = lex.nextToken() 
                        tryEat(Token.Kind.NEWLINE)

                        when (op) {
                            is Operation.OperationZero -> {
                                if (dst != null) error("Line ${idTok.line}: Opcode '$id' expects 0 operands, got ${listOfNotNull(dst,src).size}")
                                instructions.add(Instruction.InstructionZero(op, idTok.line))
                            }
                            is Operation.OperationOne -> {
                                if (dst == null || src != null) error("Line ${idTok.line}: Opcode '$id' expects 1 operand, got ${listOfNotNull(dst,src).size}")
                                instructions.add(Instruction.InstructionOne(op, dst, idTok.line))
                            }
                            is Operation.OperationTwo -> {
                                if (dst == null || src == null) error("Line ${idTok.line}: Opcode '$id' expects 2 operands, got ${listOfNotNull(dst, src).size}")
                                instructions.add(Instruction.InstructionTwo(op, dst, src, idTok.line))
                            }
                        }

                    } else if (currentSection == Section.DATA) {
                        val symbolName = id 
                        val typeTok = eat(Token.Kind.ID) 
                        val directiveText = typeTok.text.uppercase()

                        symbolTable[symbolName] = dataOffset.toUInt()
                        var valuesProcessed = 0

                        do {
                            val valueTokenForErrorLine = look.line
                            
                            if (directiveText == "DB" || directiveText == "BYTE") {
                                if (look.kind == Token.Kind.STRING) {
                                    val strToken = eat(Token.Kind.STRING)
                                    val charCodes = parseString(strToken.text)
                                    for (charCode in charCodes) {
                                        if (charCode > UByte.MAX_VALUE) error("Line ${strToken.line}: Character code $charCode (0x${charCode.toString(16)}) in string for '$symbolName' out of 8-bit range for $directiveText")
                                        mem.writeByte(dataOffset, charCode.toUByte())
                                        dataOffset += 1
                                    }
                                    if (charCodes.isNotEmpty()) valuesProcessed++ // Count the whole string as one item in a comma list
                                    else if (strToken.text.length <=2) valuesProcessed++ // handles "" or '' as one item

                                } else { // Handle single byte: number, '?', or char literal
                                    val value = parseDataValue(symbolName, directiveText, valueTokenForErrorLine)
                                    if (value > UByte.MAX_VALUE) error("Line $valueTokenForErrorLine: Value $value (0x${value.toString(16)}) out of 8-bit unsigned range for $directiveText for '$symbolName'")
                                    mem.writeByte(dataOffset, value.toUByte())
                                    dataOffset += 1
                                    valuesProcessed++
                                }
                            } else if (directiveText == "DW" || directiveText == "WORD") {
                                if (look.kind == Token.Kind.STRING) error("Line $valueTokenForErrorLine: String literals like \"${look.text}\" are not directly supported for $directiveText. Use DB/BYTE or individual char codes.")
                                val value = parseDataValue(symbolName, directiveText, valueTokenForErrorLine)
                                if (value > UShort.MAX_VALUE) error("Line $valueTokenForErrorLine: Value $value (0x${value.toString(16)}) out of 16-bit unsigned range for $directiveText for '$symbolName'")
                                mem.writeWord(dataOffset, value.toUShort())
                                dataOffset += 2
                                valuesProcessed++
                            } else if (directiveText == "DD" || directiveText == "DWORD" || directiveText == "LONG") {
                                if (look.kind == Token.Kind.STRING) error("Line $valueTokenForErrorLine: String literals like \"${look.text}\" are not directly supported for $directiveText. Use DB/BYTE or individual char codes.")
                                val value = parseDataValue(symbolName, directiveText, valueTokenForErrorLine)
                                // Value is already UInt from parseDataValue
                                mem.writeDWord(dataOffset, value)
                                dataOffset += 4
                                valuesProcessed++
                            } else {
                                error("Line ${typeTok.line}: Unsupported data directive '$directiveText'. Supported: DB, BYTE, DW, WORD, DD, DWORD, LONG")
                            }
                        } while (tryEat(Token.Kind.COMMA) != null)

                        if (valuesProcessed == 0 && !( (directiveText == "DB" || directiveText == "BYTE") && look.kind == Token.Kind.STRING && parseString(look.text).isEmpty()) ) {
                             error("Line ${typeTok.line}: Data directive '$directiveText' for '$symbolName' requires at least one value, but none were parsed or string was empty.")
                        }

                        while (!isNewlineOrEOF()) look = lex.nextToken()
                        tryEat(Token.Kind.NEWLINE)
                    }
                }
                // Handle other tokens like comments if they are tokenized separately
                // and not just consumed by "while (!isNewlineOrEOF())"
                else -> { 
                    // This case handles unexpected tokens at the start of a line,
                    // or lines that are entirely comments if your lexer emits a COMMENT token.
                    // If comments are just skipped by the lexer or consumed by `while(!isNewlineOrEOF())`
                    // in previous branches, this might not be hit often.
                    val errToken = look
                    // Consume the unexpected token and the rest of the line to prevent infinite loops
                    while (!isNewlineOrEOF()) look = lex.nextToken()
                    tryEat(Token.Kind.NEWLINE)
                    // Optionally, you could error here or log a warning if strict parsing is needed.
                    // error("Line ${errToken.line}: Unexpected token ${errToken.kind} ('${errToken.text}') at start of line or in unexpected place.")
                }
            }
        }
    }

    // Helper function to parse a single data value (number, '?', char literal)
    private fun parseDataValue(symbolName: String, directiveText: String, errorLine: Int): UInt {
        return if (look.kind == Token.Kind.ID && look.text == "?") {
            eat(Token.Kind.ID) // Consume '?'
            0u // Represents uninitialized data
        } else if (look.kind == Token.Kind.NUMBER) {
            val numToken = eat(Token.Kind.NUMBER)
            parseImmediate(numToken.text)
        } else if (look.kind == Token.Kind.ID) { // Handles char literals like 'A' or 'c' etc.
            val idValToken = eat(Token.Kind.ID)
            try {
                parseImmediate(idValToken.text) // parseImmediate handles single char literals
            } catch (e: NumberFormatException) {
                error("Line ${idValToken.line}: Invalid value '${idValToken.text}' for data directive '$directiveText' for '$symbolName'. Expected number, '?', or char literal. Error: ${e.message}")
            }
        } else {
            error("Line $errorLine: Data directive '$directiveText' for '$symbolName' expects a value (e.g., number, '?', char literal, or string for DB/BYTE) but found ${look.kind} ('${look.text}').")
        }
    }
}
