// Mini MASM-like Interpreter in Kotlin
// ---------------------------------------------------------------
// MVP scope:
// - 16-bit registers: AX, BX, CX, DX, SI, DI, BP, SP
// - Labels, comments (;), decimal and hex literals (e.g., 26, 0x1A, 1Ah)
// - Addressing modes: REG, IMM, MEM [disp], [REG], [REG+disp]
// - Word-sized operations only (no 8-bit AL/AH, etc.)
// - Instructions: MOV, ADD, SUB, INC, DEC, CMP,
//                 JMP, JE, JNE, JG, JL, JGE, JLE,
//                 PUSH, POP, CALL, RET,
//                 INT imm (INT 20h halts program)
// - Directives: (none yet) – data/build your memory manually or via MOV to [addr]
// Limitations:
// - No segments, no .data/.code, no macros, no PROC/ENDP, no DUP, no EQU.
// - All memory ops are 16-bit word-sized. Strings/bytes not supported yet.
// - Flags updated for ADD/SUB/CMP (ZF/SF/CF) – simple model.
// - IP is an instruction index (not real x86 bytes).
// ---------------------------------------------------------------

// ---------- CPU & Machine Model ----------

data class Flags(var ZF: Boolean = false, var SF: Boolean = false, var CF: Boolean = false)

enum class Reg { AX, BX, CX, DX, SI, DI, BP, SP }

data class CPU(
    val regs: MutableMap<Reg, Int> = mutableMapOf(
        Reg.AX to 0, Reg.BX to 0, Reg.CX to 0, Reg.DX to 0,
        Reg.SI to 0, Reg.DI to 0, Reg.BP to 0, Reg.SP to 0xFFFE // stack grows down
    ),
    var IP: Int = 0, // instruction pointer (index into instruction list)
    val flags: Flags = Flags()
)

class Memory(size: Int = 64 * 1024) { // 64KB
    private val mem = ByteArray(size)
    val size = mem.size

    fun read8(addr: Int): Int = mem[addr and 0xFFFF].toInt() and 0xFF
    fun write8(addr: Int, value: Int) { mem[addr and 0xFFFF] = (value and 0xFF).toByte() }

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

// ---------- IR (Instructions & Operands) ----------

enum class Op { MOV, ADD, SUB, INC, DEC, CMP, JMP, JE, JNE, JG, JL, JGE, JLE, PUSH, POP, CALL, RET, INT, NOP }

sealed class Operand {
    data class RegOp(val reg: Reg): Operand()
    data class ImmOp(val value: Int): Operand() // 16-bit immediate
    data class LabelOp(val name: String): Operand() // For symbolic labels
    data class MemOp(val base: Reg?, val disp: Int?): Operand() // [base + disp] or [disp]
}

data class Instruction(
    val op: Op,
    val dst: Operand? = null,
    val src: Operand? = null,
    val src2: Operand? = null, // (unused for now; reserved for future)
    val line: Int = -1
)

// ---------- Lexer ----------

private data class Token(val kind: Kind, val text: String, val line: Int) {
    enum class Kind { ID, NUMBER, LBRACK, RBRACK, PLUS, COMMA, COLON, NEWLINE, EOF }
}

private class Lexer(private val source: String) {
    private val lines = source.replace("\r\n", "\n").replace('\r', '\n').split('\n')
    private var lineIdx = 0
    private var colIdx = 0

    fun nextToken(): Token {
        while (true) {
            if (lineIdx >= lines.size) return Token(Token.Kind.EOF, "", lines.size)
            var line = lines[lineIdx]
            // Strip comments
            val semi = line.indexOf(';')
            if (semi >= 0) line = line.substring(0, semi)
            if (colIdx >= line.length) {
                // End of this line -> NEWLINE and advance
                lineIdx++
                colIdx = 0
                return Token(Token.Kind.NEWLINE, "", lineIdx)
            }
            val ch = line[colIdx]
            when {
                ch.isWhitespace() -> { colIdx++; continue }
                ch == '[' -> { colIdx++; return Token(Token.Kind.LBRACK, "[", lineIdx+1) }
                ch == ']' -> { colIdx++; return Token(Token.Kind.RBRACK, "]", lineIdx+1) }
                ch == '+' -> { colIdx++; return Token(Token.Kind.PLUS, "+", lineIdx+1) }
                ch == ',' -> { colIdx++; return Token(Token.Kind.COMMA, ",", lineIdx+1) }
                ch == ':' -> { colIdx++; return Token(Token.Kind.COLON, ":", lineIdx+1) }
                ch.isLetter() || ch == '_' || ch == '.' -> {
                    val start = colIdx
                    colIdx++
                    while (colIdx < line.length && (line[colIdx].isLetterOrDigit() || line[colIdx] == '_' || line[colIdx] == '.')) colIdx++
                    val text = line.substring(start, colIdx)
                    return Token(Token.Kind.ID, text, lineIdx+1)
                }
                ch.isDigit() -> {
                    val start = colIdx
                    colIdx++
                    while (colIdx < line.length && (line[colIdx].isLetterOrDigit() || line[colIdx] in setOf('x','X','h','H'))) colIdx++
                    val text = line.substring(start, colIdx)
                    return Token(Token.Kind.NUMBER, text, lineIdx+1)
                }
                else -> {
                    // Unknown char -> treat as ID to keep going, but you could error
                    colIdx++
                    return Token(Token.Kind.ID, ch.toString(), lineIdx+1)
                }
            }
        }
    }
}

// ---------- Parser ----------

class Parser(src: String) {
    private val lex = Lexer(src)
    private var look: Token = lex.nextToken()

    private fun eat(kind: Token.Kind): Token {
        if (look.kind != kind) error("Parse error at line ${look.line}: expected $kind, got ${look.kind}")
        val t = look
        look = lex.nextToken()
        return t
    }

    private fun tryEat(kind: Token.Kind): Token? {
        if (look.kind == kind) return eat(kind)
        return null
    }

    private fun isNewlineOrEOF() = look.kind == Token.Kind.NEWLINE || look.kind == Token.Kind.EOF

    private fun parseNumber(text: String, line: Int): Int {
        val t = text.lowercase()
        return when {
            t.startsWith("0x") -> t.substring(2).toInt(16)
            t.endsWith("h") -> t.dropLast(1).toInt(16)
            else -> t.toInt()
        } and 0xFFFF
    }

    private fun parseReg(id: String): Reg? = when(id.uppercase()) {
        "AX"->Reg.AX
        "BX"->Reg.BX
        "CX"->Reg.CX
        "DX"->Reg.DX
        "SI"->Reg.SI
        "DI"->Reg.DI
        "BP"->Reg.BP
        "SP"->Reg.SP
        else -> null
    }

    private fun parseOperand(): Operand {
        return when (look.kind) {
            Token.Kind.ID -> {
                val idTok = eat(Token.Kind.ID)
                val reg = parseReg(idTok.text)
                if (reg != null) return Operand.RegOp(reg)
                // If it's an identifier not a register, treat it as a symbolic label.
                return Operand.LabelOp(idTok.text)
            }
            Token.Kind.NUMBER -> {
                val nTok = eat(Token.Kind.NUMBER)
                Operand.ImmOp(parseNumber(nTok.text, nTok.line))
            }
            Token.Kind.LBRACK -> {
                eat(Token.Kind.LBRACK)
                var base: Reg? = null
                var disp: Int? = null
                when (look.kind) {
                    Token.Kind.ID -> {
                        val idTok = eat(Token.Kind.ID)
                        val r = parseReg(idTok.text)
                        if (r != null) base = r else error("Line ${idTok.line}: unknown base register ${idTok.text}")
                        if (tryEat(Token.Kind.PLUS) != null) {
                            disp = when(look.kind){
                                Token.Kind.NUMBER -> parseNumber(eat(Token.Kind.NUMBER).text, idTok.line)
                                else -> error("Line ${idTok.line}: expected displacement number after +")
                            }
                        }
                    }
                    Token.Kind.NUMBER -> {
                        disp = parseNumber(eat(Token.Kind.NUMBER).text, look.line)
                    }
                    else -> error("Line ${look.line}: bad memory operand")
                }
                eat(Token.Kind.RBRACK)
                Operand.MemOp(base, disp)
            }
            else -> error("Line ${look.line}: unexpected token ${look.kind}")
        }
    }

    private fun parseOp(id: String): Op = when(id.uppercase()) {
        "MOV"->Op.MOV
        "ADD"->Op.ADD
        "SUB"->Op.SUB
        "INC"->Op.INC
        "DEC"->Op.DEC
        "CMP"->Op.CMP
        "JMP"->Op.JMP
        "JE"->Op.JE
        "JNE"->Op.JNE
        "JG"->Op.JG
        "JL"->Op.JL
        "JGE"->Op.JGE
        "JLE"->Op.JLE
        "PUSH"->Op.PUSH
        "POP"->Op.POP
        "CALL"->Op.CALL
        "RET"->Op.RET
        "INT"->Op.INT
        "NOP"->Op.NOP
        else -> error("Line ${look.line}: unknown opcode $id")
    }

    data class ParsedProgram(
        val instructions: MutableList<Instruction>,
        val labels: MutableMap<String, Int> // label -> instruction index
    )

    fun parseProgram(): ParsedProgram {
        val instrs = mutableListOf<Instruction>()
        val labels = mutableMapOf<String, Int>()

        fun atLineStart(): Boolean = true // (not strictly tracked; fine for MVP)

        while (true) {
            when (look.kind) {
                Token.Kind.EOF -> return ParsedProgram(instrs, labels)
                Token.Kind.NEWLINE -> { eat(Token.Kind.NEWLINE); continue }
                Token.Kind.ID -> {
                    // Could be a label or an opcode
                    val idTok = eat(Token.Kind.ID)
                    if (tryEat(Token.Kind.COLON) != null) {
                        // label
                        val name = idTok.text
                        labels[name] = instrs.size // points to next instruction index
                        // consume any trailing tokens until newline
                        while (!isNewlineOrEOF()) { look = lex.nextToken() }
                        tryEat(Token.Kind.NEWLINE)
                        continue
                    } else {
                        // opcode path
                        val op = parseOp(idTok.text)
                        var dst: Operand? = null
                        var src: Operand? = null
                        if (!isNewlineOrEOF()) {
                            // dst
                            dst = parseOperand()
                            if (tryEat(Token.Kind.COMMA) != null) {
                                src = parseOperand()
                            }
                            // eat rest of line
                            while (!isNewlineOrEOF()) look = lex.nextToken()
                        }
                        tryEat(Token.Kind.NEWLINE)
                        instrs.add(Instruction(op, dst, src, null, idTok.line))
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

// ---------- Executor ----------

class Interpreter(private val program: List<Instruction>, private val labels: Map<String, Int>) {
    private val cpu = CPU()
    private val mem = Memory()

    private fun readReg(r: Reg) = cpu.regs[r]!! and 0xFFFF
    private fun writeReg(r: Reg, v: Int) { cpu.regs[r] = v and 0xFFFF }

    private fun addrOf(memOp: Operand.MemOp): Int {
        val base = memOp.base?.let { readReg(it) } ?: 0
        val disp = memOp.disp ?: 0
        return (base + disp) and 0xFFFF
    }

    private fun readOp(op: Operand, line: Int): Int = when(op) {
        is Operand.ImmOp -> op.value and 0xFFFF
        is Operand.RegOp -> readReg(op.reg)
        is Operand.MemOp -> mem.read16(addrOf(op))
        is Operand.LabelOp -> labels[op.name]?.let { it and 0xFFFF } ?: error("Line $line: Unknown label: ${op.name}")
    }

    private fun writeOp(op: Operand, value: Int, line: Int) {
        when(op) {
            is Operand.RegOp -> writeReg(op.reg, value)
            is Operand.MemOp -> mem.write16(addrOf(op), value)
            is Operand.ImmOp -> error("Line $line: cannot write to immediate")
            is Operand.LabelOp -> error("Line $line: cannot write to a label")
        }
    }

    private fun setFlagsFromAdd(a: Int, b: Int, res: Int) { // a, b are 16-bit unsigned-like values
        cpu.flags.ZF = (res and 0xFFFF) == 0
        cpu.flags.SF = (res and 0x8000) != 0
        cpu.flags.CF = (a + b) > 0xFFFF
    }

    private fun setFlagsFromSub(a: Int, b: Int, res: Int) {
        cpu.flags.ZF = (res and 0xFFFF) == 0
        cpu.flags.SF = (res and 0x8000) != 0
        cpu.flags.CF = (a - b) < 0
    }

    private fun jccTaken(op: Op): Boolean = when(op) {
        Op.JE -> cpu.flags.ZF
        Op.JNE -> !cpu.flags.ZF
        Op.JG -> (!cpu.flags.SF && !cpu.flags.ZF)
        Op.JL -> cpu.flags.SF
        Op.JGE -> !cpu.flags.SF
        Op.JLE -> (cpu.flags.SF || cpu.flags.ZF)
        else -> false
    }

    private fun push(v: Int) {
        val newSp = (readReg(Reg.SP) - 2) and 0xFFFF
        writeReg(Reg.SP, newSp)
        mem.write16(newSp, v)
    }

    private fun pop(): Int {
        val sp = readReg(Reg.SP)
        val v = mem.read16(sp)
        writeReg(Reg.SP, (sp + 2) and 0xFFFF)
        return v
    }

    fun run(maxSteps: Int = 100000) {
        var steps = 0
        while (cpu.IP in program.indices) {
            if (steps++ > maxSteps) error("Execution limit exceeded (possible infinite loop)")
            val ins = program[cpu.IP]
            // println("IP=${cpu.IP} ${ins}") // debug
            when (ins.op) {
                Op.NOP -> cpu.IP++
                Op.MOV -> {
                    val v = readOp(ins.src ?: error("MOV missing src at line ${ins.line}"), ins.line)
                    writeOp(ins.dst ?: error("MOV missing dst at line ${ins.line}"), v, ins.line)
                    cpu.IP++
                }
                Op.ADD -> {
                    val dst = ins.dst ?: error("ADD missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("ADD missing src at line ${ins.line}"), ins.line)
                    val r = (a + b) and 0xFFFF
                    setFlagsFromAdd(a, b, r)
                    writeOp(dst, r, ins.line)
                    cpu.IP++
                }
                Op.SUB -> {
                    val dst = ins.dst ?: error("SUB missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val b = readOp(ins.src ?: error("SUB missing src at line ${ins.line}"), ins.line)
                    val r = (a - b) and 0xFFFF
                    setFlagsFromSub(a, b, r)
                    writeOp(dst, r, ins.line)
                    cpu.IP++
                }
                Op.INC -> {
                    val dst = ins.dst ?: error("INC missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = (a + 1) and 0xFFFF
                    setFlagsFromAdd(a, 1, r)
                    writeOp(dst, r, ins.line)
                    cpu.IP++
                }
                Op.DEC -> {
                    val dst = ins.dst ?: error("DEC missing dst at line ${ins.line}")
                    val a = readOp(dst, ins.line)
                    val r = (a - 1) and 0xFFFF
                    setFlagsFromSub(a, 1, r)
                    writeOp(dst, r, ins.line)
                    cpu.IP++
                }
                Op.CMP -> {
                    val a = readOp(ins.dst ?: error("CMP missing left operand at line ${ins.line}"), ins.line)
                    val b = readOp(ins.src ?: error("CMP missing right operand at line ${ins.line}"), ins.line)
                    val r = (a - b) and 0xFFFF
                    setFlagsFromSub(a, b, r)
                    cpu.IP++
                }
                Op.JMP, Op.JE, Op.JNE, Op.JG, Op.JL, Op.JGE, Op.JLE -> {
                    val targetOp = ins.dst ?: ins.src ?: error("Jump missing target operand at line ${ins.line}")
                    val take = (ins.op == Op.JMP) || jccTaken(ins.op)
                    if (take) {
                        val targetIdx = readOp(targetOp, ins.line)
                        if (targetIdx !in program.indices) error("Jump target $targetIdx out of range at line ${ins.line}")
                        cpu.IP = targetIdx
                    } else {
                        cpu.IP++
                    }
                }
                Op.PUSH -> {
                    val v = readOp(ins.dst ?: error("PUSH missing operand at line ${ins.line}"), ins.line)
                    push(v)
                    cpu.IP++
                }
                Op.POP -> {
                    val dst = ins.dst ?: error("POP missing destination at line ${ins.line}")
                    val v = pop()
                    writeOp(dst, v, ins.line)
                    cpu.IP++
                }
                Op.CALL -> {
                    val targetOp = ins.dst ?: error("CALL missing target at line ${ins.line}")
                    val target = readOp(targetOp, ins.line)
                    if (target !in program.indices) error("Call target $target out of range at line ${ins.line}")
                    push(cpu.IP + 1)
                    cpu.IP = target
                }
                Op.RET -> {
                    cpu.IP = pop()
                }
                Op.INT -> {
                    val imm = readOp(ins.dst ?: error("INT requires an immediate at line ${ins.line}"), ins.line)
                    if ((imm and 0xFF) == 0x20) {
                        // INT 20h -> terminate
                        // println("Program terminated via INT 20h")
                        return
                    } else {
                        error("Unsupported interrupt $imm at line ${ins.line}")
                    }
                }
            }
        }
    }
}