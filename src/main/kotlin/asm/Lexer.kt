package asm

import model.Token

class Lexer(private val source: String) {
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
                ch.isDigit() -> {
                    val start = colIdx
                    colIdx++
                    while (colIdx < line.length && (line[colIdx].isLetterOrDigit() || line[colIdx] in setOf('x','X','h','H'))) colIdx++
                    val text = line.substring(start, colIdx)
                    return Token(Token.Kind.NUMBER, text, lineIdx + 1)
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