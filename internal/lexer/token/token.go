package token

// Type represents the type of token as a string.
type Type string

// List of token types to represent various elements of the language's syntax.
const (
	EOF        Type = "END OF FILE"   // Marks the end of the input.
	IDENTIFIER      = "IDENTIFIER"    // Represents identifiers (variable/function names).
	ILLEGAL         = "ILLEGAL"       // Marks illegal/unknown characters.
	VARTYPE         = "VARIABLE TYPE" // Represents variable types like int, float, etc.

	VARIABLE = "VARIABLE" // Keyword 'var' for variable declaration.
	FUNCTION = "FUNCTION" // Keyword 'func' for function declaration.
	CONSTANT = "CONSTANT" // Keyword 'const' for constant declaration.
	ARROW    = "ARROW"    // Represents '->' arrow for function signatures.
	IF       = "IF"       // Keyword 'if' for conditional statements.
	ELSE     = "ELSE"     // Keyword 'else' for conditional statements.
	RETURN   = "RETURN"   // Keyword 'return' for returning values from functions.

	INTEGER = "INTEGER" // Represents integer literals.
	FLOAT   = "FLOAT"   // Represents float literals.
	STRING  = "STRING"  // Represents string literals.

	TRUE  = "TRUE"  // Boolean literal 'true'.
	FALSE = "FALSE" // Boolean literal 'false'.

	NEGATION = "NEGATION" // Represents negation operator '!'.

	ASSIGN    = "ASSIGN"           // Assignment operator '='.
	EQUAL     = "EQUAL"            // Equality operator '=='.
	NOTEQUAL  = "NOT EQUAL"        // Not equal operator '!='.
	LESS      = "LESS"             // Less than operator '<'.
	LESSEQ    = "LESS OR EQUAL"    // Less than or equal to operator '<='.
	GREATER   = "GREATER"          // Greater than operator '>'.
	GREATEREQ = "GREATER OR EQUAL" // Greater than or equal to operator '>='.

	PLUS      = "PLUS"      // Addition operator '+'.
	MINUS     = "MINUS"     // Subtraction operator '-'.
	INCREMENT = "INCREMENT" // Increment operator '++'.
	DECREMENT = "DECREMENT" // Decrement operator '--'.

	ASTERISK = "ASTERISK" // Multiplication operator '*'.
	POWER    = "POWER"    // Exponentiation operator '^'.
	SLASH    = "SLASH"    // Division operator '/'.

	LPAREN    = "LEFT PAREN"    // Left parenthesis '('.
	RPAREN    = "RIGHT PAREN"   // Right parenthesis ')'.
	LBRACE    = "LEFT BRACE"    // Left brace '{'.
	RBRACE    = "RIGHT BRACE"   // Right brace '}'.
	LBRACKET  = "LEFT BRACKET"  // Left bracket '['.
	RBRACKET  = "RIGHT BRACKET" // Right bracket ']'.
	COLON     = "COLON"         // Colon ':'.
	SEMICOLON = "SEMICOLON"     // Semicolon ';'.
	COMMA     = "COMMA"         // Comma ','.
)

// Token represents a single lexical token with its type and literal value.
type Token struct {
	Type  Type   // The type of the token (e.g., IDENTIFIER, INTEGER, etc.).
	Value string // The literal value of the token (e.g., variable name, number).
}

// keywords maps string literals to specific token types for reserved keywords.
var keywords = map[string]Type{
	"var":    VARIABLE,
	"const":  CONSTANT,
	"func":   FUNCTION,
	"->":     ARROW,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
	"int":    VARTYPE,
	"float":  VARTYPE,
	"string": VARTYPE,
	"bool":   VARTYPE,
}

// LookupIdentifier checks if an identifier is a reserved keyword or a general identifier.
func LookupIdentifier(identifier string) Type {
	// If the identifier is a keyword, return its token type. Otherwise, return IDENTIFIER.
	if token, exists := keywords[identifier]; exists {
		return token
	}
	return IDENTIFIER
}
