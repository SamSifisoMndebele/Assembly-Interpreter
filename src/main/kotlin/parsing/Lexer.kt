package parsing

import isa.Mnemonic
import kotlin.reflect.full.companionObjectInstance

/**
 * The Lexer class is responsible for tokenizing the input source code.
 * It takes a string of source code as input and produces a stream of tokens.
 *
 * The lexer works by iterating through the source code line by line,
 * and for each line, it tries to match various patterns to identify tokens.
 * Comments (lines starting at the beginning of the line with ';') are ignored.
 *
 * The supported token types include:
 * - Numbers (hexadecimal, binary, octal, decimal, real)
 * - Identifiers
 * - Labels
 * - Registers
 * - Segments (code, data, stack)
 * - Strings
 * - Data directives (byte, word, dword, etc.)
 * - Operations (instructions)
 * - Punctuation (comma, brackets, parentheses, plus, minus, multiply, divide)
 * - Expressions (e.g. 1+5*2)
 *
 * If an unrecognized character is encountered, it is treated as an UNKNOWN token.
 *
 * @property source The input source code string.
 */
abstract class Lexer(source: String) {
    companion object {
        private val operations = Mnemonic.allMnemonics.map { it.toString().lowercase() }
    }

    private val tokens = mutableListOf<Token>()
    private val labels = mutableSetOf<String>()

    init {
        // Split the source into lines and remove comments
        val lines = source
            .replace("\r\n", "\n")
            .replace('\r', '\n')
            .split('\n')
            .map { it.substringBefore(';').trim() }

        // Tokenize each line
        for ((i, string) in lines.withIndex()) {
            var line = string
            val lineNumber = i + 1
            while (line.isNotEmpty()) {
                val token = line.nextToken(lineNumber) ?: error("Unknown token at line $lineNumber: '$line'")
                tokens.add(token)
                line = line.substring(token.length).trim()
                if (token is Token.DataDir) while (line.startsWith('(') && line.endsWith(')')) {
                    line = line.substring(1, line.length - 1).trim()
                }
            }
        }
    }

    private inline fun <reified T : Token> String.match(line: Int): T? {
        val factory = T::class.companionObjectInstance as Token.Factory
        val matcher = factory.pattern.matcher(this)
        return if (matcher.find()) {
            factory.create(matcher.group().lowercase(), line) as T
        } else null
    }

    private fun String.nextToken(line: Int): Token? {
        match<Token.Segment>(line)?.let { return it }
        match<Token.DataDir>(line)?.let { return it }
        match<Token.Label>(line)?.let {
            if (!labels.add(it.text)) {
                error("Syntax Error: Label '${it.text}' is already defined. Cannot redefine at line $line")
            }
            return it
        }
        match<Token.Uninitialized>(line)?.let { return it }
        match<Token.Text>(line)?.let { return it }
        nextPunctuationToken(line)?.let { return it }
        match<Token.RealNumber>(line)?.let { return it }
        match<Token.HexNumber>(line)?.let { return it }
        match<Token.BinNumber>(line)?.let { return it }
        match<Token.OctNumber>(line)?.let { return it }
        match<Token.DecNumber>(line)?.let { return it }
        match<Token.Register>(line)?.let { return it }
        match<Token.Identifier>(line)?.let {
            return if (operations.contains(it.text.lowercase())) Token.Mnemonic(it.text, it.line)
            else it
        }

        return null
    }

    private fun String.nextPunctuationToken(line: Int): Token? {
        match<Token.Comma>(line)?.let { return it }
        match<Token.LBracket>(line)?.let { return it }
        match<Token.RBracket>(line)?.let { return it }
        match<Token.LParen>(line)?.let { return it }
        match<Token.RParen>(line)?.let { return it }
        match<Token.Plus>(line)?.let { return it }
        match<Token.Minus>(line)?.let { return it }
        match<Token.Multi>(line)?.let { return it }
        match<Token.Div>(line)?.let { return it }
        return null
    }

    private var index = -1

    /**
     * Checks if there are more tokens to be processed.
     *
     * @return `true` if there are more tokens, `false` otherwise.
     */
    fun hasToken(): Boolean = index < tokens.size - 1

    /**
     * Returns the next token from the input source code.
     *
     * @return The next token, or throws an exception if there are no more tokens.
     * @throws NoSuchElementException if there are no more tokens.
     */
    fun nextToken(): Token {
        if (index >= tokens.size) throw NoSuchElementException()
        return tokens[++index]
    }

    /**
     * Peeks at the next token in the stream without consuming it.
     * This function allows you to look ahead at the next token that would be returned by [nextToken],
     * without advancing the iterator.
     *
     * @return The next token in the stream.
     * @throws NoSuchElementException if there are no more tokens.
     */
    fun peekToken(): Token {
        if (index >= tokens.size) throw NoSuchElementException()
        return tokens[index + 1]
    }

    /**
     * Returns the previous token from the input source code.
     * This function is useful when you need to look back at the previously processed token.
     *
     * @return The previous token.
     * @throws NoSuchElementException if there is no previous token (e.g., at the beginning of the token stream).
     */
    fun previousToken(): Token {
        if (index <= 0) throw NoSuchElementException()
        return tokens[--index]
    }

    /**
     * Checks if there is a previous token.
     *
     * This function is useful when iterating backwards or when needing to look behind the current token.
     *
     * @return `true` if there is a previous token, `false` otherwise.
     */
    fun hasPrevious(): Boolean = index > 0

    /**
     * Returns a list of all tokens generated from the input source code.
     *
     * @return A list of [Token] objects.
     */
    fun getTokens(): List<Token> = tokens

    fun toDataSegment(): Boolean {
        index = -1
        while (++index < tokens.size) {
            val token = tokens[index]
            if (token is Token.Segment && token.text.contains("data", ignoreCase = true)) {
                return true
            }
        }
        return false
    }

    fun toCodeSegment(): Boolean {
        index = -1
        while (++index < tokens.size) {
            val token = tokens[index]
            if (token is Token.Segment && token.text.contains("code", ignoreCase = true)) {
                return true
            }
        }
        return false
    }

    fun toStackSegment(): Boolean {
        index = -1
        while (++index < tokens.size) {
            val token = tokens[index]
            if (token is Token.Segment && token.text.contains("stack", ignoreCase = true)) {
                return true
            }
        }
        return false
    }
}
