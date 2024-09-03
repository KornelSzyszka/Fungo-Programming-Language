package parser

import (
	"Fungo/internal/lexer"
	"fmt"
	"strconv"
)

const (
	LOWEST = iota
	EQUALS
	LESSGREATER
	SUM
	PRODUCT
	PREFIX
	CALL
)

type (
	prefixParser func() Expression
	infixParser  func(Expression) Expression
)

type Parser struct {
	lexer_       *lexer.Lexer
	currentToken lexer.Token
	nextToken    lexer.Token
	errors       []string
	prefixParser map[lexer.TokenType]prefixParser
	infixParser  map[lexer.TokenType]infixParser
}

func New(lexer_ *lexer.Lexer) *Parser {
	parser := &Parser{
		lexer_: lexer_,
		errors: []string{},
	}

	parser.prefixParser = make(map[lexer.TokenType]prefixParser)
	parser.registerPrefix(lexer.IDENTIFIER, parser.parseIdentifier)
	parser.registerPrefix(lexer.INTEGER, parser.parseIntegerLiteral)
	parser.registerPrefix(lexer.NEGATION, parser.parsePrefixExpression)
	parser.registerPrefix(lexer.MINUS, parser.parsePrefixExpression)

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
	case lexer.RETURN:
		return parser.parseReturnStatement()
	default:
		return parser.parseExpressionStatement()
	}
}

func (parser *Parser) registerPrefix(tokenType lexer.TokenType, prefix prefixParser) {
	parser.prefixParser[tokenType] = prefix
}

func (parser *Parser) registerInfix(tokenType lexer.TokenType, infix infixParser) {
	parser.infixParser[tokenType] = infix
}

func (parser *Parser) parseIdentifier() Expression {
	return &Identifier{Token: parser.currentToken, Value: parser.currentToken.Value}
}

func (parser *Parser) parseIntegerLiteral() Expression {
	integerLiteral := &IntegerLiteral{Token: parser.currentToken}

	value, err := strconv.ParseInt(parser.currentToken.Value, 10, 64)
	if err != nil {
		message := fmt.Sprintf("could not parse %q as integer", parser.currentToken.Value)
		parser.errors = append(parser.errors, message)
		return nil
	}

	integerLiteral.Value = value

	return integerLiteral
}

func (parser *Parser) parseExpressionStatement() *ExpressionStatement {
	statement := &ExpressionStatement{Token: parser.currentToken}

	statement.Expression = parser.parseExpression(LOWEST)

	if parser.nextTokenIs(lexer.SEMICOLON) {
		parser.getNextToken()
	}

	return statement
}

func (parser *Parser) parseExpression(precedence int) Expression {
	prefix := parser.prefixParser[parser.currentToken.Type]

	if prefix == nil {
		parser.noPrefixParseFnError(parser.currentToken.Type)
		return nil
	}

	return prefix()
}

func (parser *Parser) parsePrefixExpression() Expression {
	expression := &PrefixExpression{
		Token:    parser.currentToken,
		Operator: parser.currentToken.Value,
	}

	parser.getNextToken()

	expression.Right = parser.parseExpression(PREFIX)

	return expression
}

func (parser *Parser) noPrefixParseFnError(tokenType lexer.TokenType) {
	message := fmt.Sprintf("no prefix parse function for %s found", tokenType)
	parser.errors = append(parser.errors, message)
}

func (parser *Parser) parseVarStatement() Statement {
	statement := &VarStatement{Token: parser.currentToken}

	if !parser.expectNext(lexer.IDENTIFIER) {
		return nil
	}

	statement.Name = &Identifier{Token: parser.currentToken, Value: parser.currentToken.Value}

	if !parser.expectNext(lexer.COLON) {
		return nil
	}
	if !parser.expectNext(lexer.VARTYPE) {
		return nil
	}

	statement.VarType = &VarType{Token: parser.currentToken, Value: parser.currentToken.Value}

	if !parser.expectNext(lexer.ASSIGN) {
		return nil
	}

	for !parser.currentTokenIs(lexer.SEMICOLON) {
		parser.getNextToken()
	}

	return statement
}

func (parser *Parser) parseReturnStatement() Statement {
	statement := &ReturnStatement{Token: parser.currentToken}

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
