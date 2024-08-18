package lexer

import (
	"unicode"
)

type Lexer struct {
	input        string
	position     int  // position in input
	readPosition int  // after current char
	currentChar  rune // current char
}

func New(input string) *Lexer {
	lexer := &Lexer{input: input}
	lexer.readChar()
	return lexer
}

func (lexer *Lexer) readChar() {
	lexer.currentChar = lexer.getChar()
	lexer.position = lexer.readPosition
	lexer.readPosition++
}

func (lexer *Lexer) NextToken() Token {
	lexer.skipWhitespace()

	var token_ Token
	// TODO: Add arrow
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
		token_ = Token{Type: ILLEGAL, Value: string(lexer.currentChar)}
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

func isLetter(char rune) bool {
	return 'a' <= char && char <= 'z' || 'A' <= char && char <= 'Z' || char == '_'
}

func (lexer *Lexer) getChar() rune {
	nextChar := rune(0)
	if lexer.readPosition < len(lexer.input) {
		nextChar = rune(lexer.input[lexer.readPosition])
	}
	return nextChar
}

func (lexer *Lexer) skipWhitespace() {
	for lexer.currentChar == ' ' || lexer.currentChar == '\t' ||
		lexer.currentChar == '\n' || lexer.currentChar == '\r' {
		lexer.readChar()
	}
}

func (lexer *Lexer) readIdentifier() string {
	start := lexer.position
	for isLetter(lexer.currentChar) {
		lexer.readChar()
	}
	return lexer.input[start:lexer.position]
}

// TODO: add other number types
func (lexer *Lexer) readNumber() string {
	start := lexer.position
	for unicode.IsDigit(lexer.currentChar) {
		lexer.readChar()
	}
	return lexer.input[start:lexer.position]
}
