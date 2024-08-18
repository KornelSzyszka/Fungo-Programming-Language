package parser

import (
	"Fungo/internal/lexer"
	"fmt"
)

type Parser struct {
	lexer_       *lexer.Lexer
	currentToken lexer.Token
	nextToken    lexer.Token
	errors       []string
}

func New(lexer_ *lexer.Lexer) *Parser {
	parser := &Parser{
		lexer_: lexer_,
		errors: []string{},
	}

	parser.getNextToken()
	parser.getNextToken()

	return parser
}

func (parser *Parser) ParseProgram() *Program {
	program := &Program{}
	program.Statements = []Statement{}

	for parser.currentToken.Type != lexer.EOF {
		statement := parser.parseStatement()
		if statement != nil {
			program.Statements = append(program.Statements, statement)
		}
		parser.getNextToken()
	}

	return program
}

func (parser *Parser) Errors() []string {
	return parser.errors
}

func (parser *Parser) nextTokenError(tokenType lexer.TokenType) {
	message := fmt.Sprintf("expected next token to be %s, got %s instead", tokenType, parser.currentToken.Type)
	parser.errors = append(parser.errors, message)
}

func (parser *Parser) getNextToken() {
	parser.currentToken = parser.nextToken
	parser.nextToken = parser.lexer_.NextToken()
}

func (parser *Parser) parseStatement() Statement {
	switch parser.currentToken.Type {
	case lexer.VARIABLE:
		return parser.parseVarStatement()
	default:
		return nil
	}
}

func (parser *Parser) parseVarStatement() Statement {
	statement := &VarStatement{Token: parser.currentToken}

	if !parser.expectNext(lexer.IDENTIFIER) {
		return nil
	}

	statement.Name = &Identifier{Token: parser.currentToken, Value: parser.currentToken.Value}

	if !parser.expectNext(lexer.ASSIGN) {
		return nil
	}

	for !parser.currentTokenIs(lexer.SEMICOLON) {
		parser.getNextToken()
	}

	return statement
}

func (parser *Parser) expectNext(tokenType lexer.TokenType) bool {
	if parser.nextTokenIs(tokenType) {
		parser.getNextToken()
		return true
	} else {
		parser.nextTokenError(tokenType)
		return false
	}
}

func (parser *Parser) currentTokenIs(tokenType lexer.TokenType) bool {
	return parser.currentToken.Type == tokenType
}

func (parser *Parser) nextTokenIs(tokenType lexer.TokenType) bool {
	return parser.nextToken.Type == tokenType
}
