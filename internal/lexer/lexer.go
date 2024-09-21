package lexer

import (
	"unicode"
)

// Lexer represents a lexical analyzer for tokenizing input strings.
// It tracks the current position, read position, and current character.
type Lexer struct {
	input        string // Input string to be tokenized
	position     int    // Current position in input
	readPosition int    // Position after the current character
	currentChar  rune   // Current character being processed
}

// New creates a new Lexer instance for a given input string.
// It initializes the lexer and reads the first character.
func New(input string) *Lexer {
	lexer := &Lexer{input: input}
	lexer.readChar()
	return lexer
}

// readChar advances the lexer to the next character in the input string.
// It updates the current character, position, and readPosition.
func (lexer *Lexer) readChar() {
	lexer.currentChar = lexer.getChar()
	lexer.position = lexer.readPosition
	lexer.readPosition++
}

// NextToken returns the next token from the input string.
// It skips whitespaces and processes different token types, such as operators,
// delimiters, numbers, and identifiers. It also handles multi-character tokens.
func (lexer *Lexer) NextToken() Token {
	lexer.skipWhitespace()

	var token_ Token

	switch lexer.currentChar {
	case '=':
		token_ = Token{Type: ASSIGN, Value: string(lexer.currentChar)}
		if lexer.getChar() == '=' {
			token_ = Token{
				Type:  EQUAL,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '!':
		token_ = Token{Type: NEGATION, Value: string(lexer.currentChar)}
		if lexer.getChar() == '=' {
			token_ = Token{
				Type:  NOTEQUAL,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '>':
		token_ = Token{Type: GREATER, Value: string(lexer.currentChar)}
		if lexer.getChar() == '=' {
			token_ = Token{
				Type:  GREATEREQ,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '<':
		token_ = Token{Type: LESS, Value: string(lexer.currentChar)}
		if lexer.getChar() == '=' {
			token_ = Token{
				Type:  LESSEQ,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '+':
		token_ = Token{Type: PLUS, Value: string(lexer.currentChar)}
		if lexer.getChar() == '+' {
			token_ = Token{
				Type:  INCREMENT,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '-':
		token_ = Token{Type: MINUS, Value: string(lexer.currentChar)}
		if lexer.getChar() == '-' {
			token_ = Token{
				Type:  DECREMENT,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
		if lexer.getChar() == '>' {
			token_ = Token{
				Type:  ARROW,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '*':
		token_ = Token{Type: ASTERISK, Value: string(lexer.currentChar)}
		if lexer.getChar() == '*' {
			token_ = Token{
				Type:  POWER,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '/':
		token_ = Token{Type: SLASH, Value: string(lexer.currentChar)}
	case '(':
		token_ = Token{Type: LPAREN, Value: string(lexer.currentChar)}
	case ')':
		token_ = Token{Type: RPAREN, Value: string(lexer.currentChar)}
	case '{':
		token_ = Token{Type: LBRACE, Value: string(lexer.currentChar)}
	case '}':
		token_ = Token{Type: RBRACE, Value: string(lexer.currentChar)}
	case '[':
		token_ = Token{Type: LBRACKET, Value: string(lexer.currentChar)}
	case ']':
		token_ = Token{Type: RBRACKET, Value: string(lexer.currentChar)}
	case ':':
		token_ = Token{Type: COLON, Value: string(lexer.currentChar)}
	case ';':
		token_ = Token{Type: SEMICOLON, Value: string(lexer.currentChar)}
	case ',':
		token_ = Token{Type: COMMA, Value: string(lexer.currentChar)}
	case 0:
		token_ = Token{Type: EOF, Value: ""}
	default:
		token_ = Token{Type: ILLEGAL, Value: string(lexer.currentChar)}

		if isLetter(lexer.currentChar) {
			token_ = Token{Type: IDENTIFIER, Value: lexer.readIdentifier()}
			token_.Type = LookupIdentifier(token_.Value)
			return token_
		}

		if unicode.IsDigit(lexer.currentChar) {
			token_ = Token{Type: INTEGER, Value: lexer.readNumber()}
			return token_
		}
	}
	lexer.readChar()
	return token_
}

// isLetter checks whether a given character is a letter (a-z, A-Z) or an underscore ('_').
func isLetter(char rune) bool {
	return 'a' <= char && char <= 'z' || 'A' <= char && char <= 'Z' || char == '_'
}

// getChar returns the next character in the input string without advancing the position.
// If the end of the input is reached, it returns 0 (EOF).
func (lexer *Lexer) getChar() rune {
	nextChar := rune(0)
	if lexer.readPosition < len(lexer.input) {
		nextChar = rune(lexer.input[lexer.readPosition])
	}
	return nextChar
}

// skipWhitespace skips over any whitespace characters (spaces, tabs, newlines).
func (lexer *Lexer) skipWhitespace() {
	for lexer.currentChar == ' ' || lexer.currentChar == '\t' ||
		lexer.currentChar == '\n' || lexer.currentChar == '\r' {
		lexer.readChar()
	}
}

// readIdentifier reads an identifier from the input string.
// It continues reading until a non-letter character is encountered.
func (lexer *Lexer) readIdentifier() string {
	start := lexer.position
	for isLetter(lexer.currentChar) {
		lexer.readChar()
	}
	return lexer.input[start:lexer.position]
}

// readNumber reads a sequence of digits (an integer) from the input string.
// It continues reading until a non-digit character is encountered.
func (lexer *Lexer) readNumber() string {
	start := lexer.position
	for unicode.IsDigit(lexer.currentChar) {
		lexer.readChar()
	}
	return lexer.input[start:lexer.position]
}
