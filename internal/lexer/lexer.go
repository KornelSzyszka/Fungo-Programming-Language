package lexer

import (
	"Fungo/internal/lexer/token"
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
func (lexer *Lexer) NextToken() token.Token {
	lexer.skipWhitespace()

	var token_ token.Token

	switch lexer.currentChar {
	case '=':
		token_ = token.Token{Type: token.ASSIGN, Value: string(lexer.currentChar)}
		if lexer.getChar() == '=' {
			token_ = token.Token{
				Type:  token.EQUAL,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '!':
		token_ = token.Token{Type: token.NEGATION, Value: string(lexer.currentChar)}
		if lexer.getChar() == '=' {
			token_ = token.Token{
				Type:  token.NOTEQUAL,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '>':
		token_ = token.Token{Type: token.GREATER, Value: string(lexer.currentChar)}
		if lexer.getChar() == '=' {
			token_ = token.Token{
				Type:  token.GREATEREQ,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '<':
		token_ = token.Token{Type: token.LESS, Value: string(lexer.currentChar)}
		if lexer.getChar() == '=' {
			token_ = token.Token{
				Type:  token.LESSEQ,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '+':
		token_ = token.Token{Type: token.PLUS, Value: string(lexer.currentChar)}
		if lexer.getChar() == '+' {
			token_ = token.Token{
				Type:  token.INCREMENT,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '-':
		token_ = token.Token{Type: token.MINUS, Value: string(lexer.currentChar)}
		if lexer.getChar() == '-' {
			token_ = token.Token{
				Type:  token.DECREMENT,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
		if lexer.getChar() == '>' {
			token_ = token.Token{
				Type:  token.ARROW,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '*':
		token_ = token.Token{Type: token.ASTERISK, Value: string(lexer.currentChar)}
		if lexer.getChar() == '*' {
			token_ = token.Token{
				Type:  token.POWER,
				Value: string(lexer.currentChar) + string(lexer.getChar()),
			}
			lexer.readChar()
		}
	case '/':
		token_ = token.Token{Type: token.SLASH, Value: string(lexer.currentChar)}
	case '(':
		token_ = token.Token{Type: token.LPAREN, Value: string(lexer.currentChar)}
	case ')':
		token_ = token.Token{Type: token.RPAREN, Value: string(lexer.currentChar)}
	case '{':
		token_ = token.Token{Type: token.LBRACE, Value: string(lexer.currentChar)}
	case '}':
		token_ = token.Token{Type: token.RBRACE, Value: string(lexer.currentChar)}
	case '[':
		token_ = token.Token{Type: token.LBRACKET, Value: string(lexer.currentChar)}
	case ']':
		token_ = token.Token{Type: token.RBRACKET, Value: string(lexer.currentChar)}
	case ':':
		token_ = token.Token{Type: token.COLON, Value: string(lexer.currentChar)}
	case ';':
		token_ = token.Token{Type: token.SEMICOLON, Value: string(lexer.currentChar)}
	case ',':
		token_ = token.Token{Type: token.COMMA, Value: string(lexer.currentChar)}
	case 0:
		token_ = token.Token{Type: token.EOF, Value: ""}
	default:
		token_ = token.Token{Type: token.ILLEGAL, Value: string(lexer.currentChar)}

		if isLetter(lexer.currentChar) {
			token_ = token.Token{Type: token.IDENTIFIER, Value: lexer.readIdentifier()}
			token_.Type = token.LookupIdentifier(token_.Value)
			return token_
		}

		if unicode.IsDigit(lexer.currentChar) {
			token_ = token.Token{Type: token.INTEGER, Value: lexer.readNumber()}
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
