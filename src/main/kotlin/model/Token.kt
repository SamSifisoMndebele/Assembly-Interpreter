package model

data class Token(val kind: Kind, val text: String, val line: Int) {
    enum class Kind { ID, NUMBER, LBRACK, RBRACK, PLUS, COMMA, COLON, NEWLINE, EOF }
}