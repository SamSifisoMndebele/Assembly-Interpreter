package asm

import model.Token
import java.util.regex.Pattern

/**
 * The Lexer class is responsible for tokenizing the input assembly source code.
 * It breaks down the source code into a stream of tokens that can be processed by the parser.
 *
 * The lexer supports various token types, including identifiers, numbers (decimal, hexadecimal, octal),
 * operators, delimiters, and keywords. It also handles comments and newlines.
 *
 * The lexer supports 8-bit, 16-bit, and 32-bit number literals.
 *  - 8-bit numbers are represented as `db` (define byte).
 *  - 16-bit numbers are represented as `dw` (define word).
 *  - 32-bit numbers are represented as `dd` (define double word).
 *
 * @property source The input assembly source code as a string.
 */
class Lexer(private val source: String) {
    private val lines = source.replace("\r\n", "\n").replace('\r', '\n').split('\n')
    private var lineIdx = 0
    private var colIdx = 0

    // Regex patterns for number literals
    private val hexPattern = Pattern.compile("^(?:0[xX][0-9a-fA-F]+|[0-9a-fA-F]+[hH])")
    private val binPattern = Pattern.compile("^(?:0[bB][01]+|[01]+[bB])")
    private val octPattern = Pattern.compile("^(?:0[qQoO][0-7]+|[0-7]+[qQoO])")
    private val decPattern = Pattern.compile("^[-+]?[0-9]+(?:_[0-9]+)*")

    /**
     * Returns the next token from the input source code.
     *
     * @return The next token, or an EOF token if the end of the input is reached.
     */
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
                ch == '[' -> { colIdx++; return Token(Token.Kind.LBRACK, "[", lineIdx + 1)
                }
                ch == ']' -> { colIdx++; return Token(Token.Kind.RBRACK, "]", lineIdx + 1)
                }
                ch == '+' -> { colIdx++; return Token(Token.Kind.PLUS, "+", lineIdx + 1)
                }
                ch == ',' -> { colIdx++; return Token(Token.Kind.COMMA, ",", lineIdx + 1)
                }
                ch == ':' -> { colIdx++; return Token(Token.Kind.COLON, ":", lineIdx + 1)
                }
                ch.isLetter() || ch == '_' || ch == '.' -> {
                    val start = colIdx
                    colIdx++
                    while (colIdx < line.length && (line[colIdx].isLetterOrDigit() || line[colIdx] == '_' || line[colIdx] == '.')) colIdx++
                    val text = line.substring(start, colIdx)
                    return Token(Token.Kind.ID, text, lineIdx + 1)
                }
                ch.isDigit() || (ch == '-' && colIdx + 1 < line.length && line[colIdx+1].isDigit()) -> {
                    // Number: decimal, hex, or binary
                    val start = colIdx
                    // Try to match hex
                    var m = hexPattern.matcher(line.substring(start))
                    if (m.lookingAt()) {
                        colIdx += m.end()
                        return Token(Token.Kind.NUMBER, line.substring(start, colIdx), lineIdx + 1)
                    }
                    // Try to match binary
                    m = binPattern.matcher(line.substring(start))
                    if (m.lookingAt()) {
                        colIdx += m.end()
                        return Token(Token.Kind.NUMBER, line.substring(start, colIdx), lineIdx + 1)
                    }
                    // Try to match octal
                    m = octPattern.matcher(line.substring(start))
                    if (m.lookingAt()) {
                        colIdx += m.end()
                        return Token(Token.Kind.NUMBER, line.substring(start, colIdx), lineIdx + 1)
                    }
                    // Then try decimal
                    m = decPattern.matcher(line)
                    if (m.find(start) && m.start() == start) {
                        colIdx = m.end()
                        return Token(Token.Kind.NUMBER, line.substring(start, colIdx), lineIdx + 1)
                    }
                    // If none, then it's an error or single digit ID.
                    // This part should ideally not be reached if patterns are comprehensive
                    colIdx++
                    return Token(Token.Kind.ID, line.substring(start, colIdx), lineIdx + 1) // Treat as ID for now
                }
                else -> {
                    // Unknown char -> treat as ID to keep going, but you could error
                    colIdx++
                    return Token(Token.Kind.ID, ch.toString(), lineIdx + 1)
                }
            }
        }
    }
}