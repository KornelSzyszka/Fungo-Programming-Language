package lexer

type TokenType string

const (
	EOF        TokenType = "END OF FILE"
	IDENTIFIER           = "IDENTIFIER"
	ILLEGAL              = "ILLEGAL"

	VARIABLE = "VARIABLE"
	FUNCTION = "FUNCTION"
	CONSTANT = "CONSTANT"
	ARROW    = "ARROW"
	IF       = "IF"
	ELSE     = "ELSE"
	RETURN   = "RETURN"

	INTEGER = "INTEGER"

	TRUE  = "TRUE"
	FALSE = "FALSE"

	ASSIGN    = "ASSIGN"
	EQUAL     = "EQUAL"
	NOTEQUAL  = "NOT EQUAL"
	LESS      = "LESS"
	LESSEQ    = "LESS OR EQUAL"
	GREATER   = "GREATER"
	GREATEREQ = "GREATER OR EQUAL"

	PLUS      = "PLUS"
	MINUS     = "MINUS"
	INCREMENT = "INCREMENT"
	DECREMENT = "DECREMENT"

	ASTERISK = "ASTERISK"
	POWER    = "POWER"
	SLASH    = "SLASH"

	LPAREN    = "LEFT PAREN"
	RPAREN    = "RIGHT PAREN"
	LBRACE    = "LEFT BRACE"
	RBRACE    = "RIGHT BRACE"
	LBRACKET  = "LEFT BRACKET"
	RBRACKET  = "RIGHT BRACKET"
	COLON     = "COLON"
	SEMICOLON = "SEMICOLON"
	COMMA     = "COMMA"
)

type Token struct {
	Type  TokenType
	Value string
}

var keywords = map[string]TokenType{
	"var":    VARIABLE,
	"const":  CONSTANT,
	"func":   FUNCTION,
	"->":     ARROW,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
}

func LookupIdentifier(identifier string) TokenType {
	if token, exists := keywords[identifier]; exists {
		return token
	}
	return IDENTIFIER
}
