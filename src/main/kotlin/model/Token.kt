package model

/**
 * Represents a token in the source code.
 *
 * A token is a sequence of characters that is treated as a single unit in the grammar of the programming language.
 * For example, keywords, identifiers, operators, literals, and punctuation marks are all tokens.
 *
 * @property kind The type of the token. See [Kind] for possible values.
 * @property text The actual text of the token as it appeared in the source code.
 * @property line The line number in the source code where this token begins.
 */
data class Token(val kind: Kind, val text: String, val line: Int) {
    /**
     * Enum representing the different kinds of tokens that can be recognized by the lexer.
     */
    enum class Kind {
        /** Identifier (e.g., variable names, function names) */
        ID,
        /** Numeric literal (supports 8-bit, 16-bit, and 32-bit representations) */
        NUMBER,
        /** String literal */
        STRING,
        /** Left bracket '[' */
        LBRACK,
        /** Right bracket ']' */
        RBRACK,
        /** Plus operator '+' */
        PLUS,
        /** Comma separator ',' */
        COMMA,
        /** Colon separator ':' */
        COLON,
        /** Newline character */
        NEWLINE,
        /** End-of-file marker, indicating no more tokens */
        EOF
    }
}