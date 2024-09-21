package parser

import (
	"Fungo/internal/lexer"
	"fmt"
	"strconv"
)

const (
	LOWEST        = iota // LOWEST represents the lowest precedence level.
	EQUALS               // EQUALS represents the precedence level for equality operators (==, !=).
	LESSGREATEREQ        // LESSGREATEREQ represents the precedence for <= and >=.
	LESSGREATER          // LESSGREATER represents the precedence for < and >.
	SUM                  // SUM represents the precedence level for addition and subtraction.
	PRODUCT              // PRODUCT represents the precedence level for multiplication and division.
	EXPONENT             // EXPONENT represents the precedence for exponentiation.
	PREFIX               // PREFIX represents the precedence level for prefix operators (-, !).
	CALL                 // CALL represents the precedence level for function calls.
)

// precedences maps token types to their corresponding precedence levels.
var precedences = map[lexer.TokenType]int{
	lexer.EQUAL:     EQUALS,
	lexer.NOTEQUAL:  EQUALS,
	lexer.LESS:      LESSGREATER,
	lexer.GREATER:   LESSGREATER,
	lexer.LESSEQ:    LESSGREATEREQ,
	lexer.GREATEREQ: LESSGREATEREQ,
	lexer.PLUS:      SUM,
	lexer.MINUS:     SUM,
	lexer.SLASH:     PRODUCT,
	lexer.ASTERISK:  PRODUCT,
	lexer.POWER:     EXPONENT,
}

type (
	// prefixParser defines a function type that parses prefix expressions.
	prefixParser func() Expression

	// infixParser defines a function type that parses infix expressions.
	infixParser func(Expression) Expression
)

// Parser represents the main structure for parsing tokens from the lexer and building an AST.
type Parser struct {
	lexer_       *lexer.Lexer                     // lexer_ is the lexer that provides the tokens.
	currentToken lexer.Token                      // currentToken is the current token being parsed.
	nextToken    lexer.Token                      // nextToken is the next token to be parsed.
	errors       []string                         // errors stores a list of parsing errors.
	prefixParser map[lexer.TokenType]prefixParser // prefixParser maps token types to their prefix parsers.
	infixParser  map[lexer.TokenType]infixParser  // infixParser maps token types to their infix parsers.
}

// New creates a new parser instance with a given lexer and initializes the prefix and infix parsers.
func New(lexer_ *lexer.Lexer) *Parser {
	parser := &Parser{
		lexer_: lexer_,
		errors: []string{},
	}

	// Register prefix parsers.
	parser.prefixParser = make(map[lexer.TokenType]prefixParser)
	parser.registerPrefix(lexer.IDENTIFIER, parser.parseIdentifier)
	parser.registerPrefix(lexer.INTEGER, parser.parseIntegerLiteral)
	parser.registerPrefix(lexer.NEGATION, parser.parsePrefixExpression)
	parser.registerPrefix(lexer.MINUS, parser.parsePrefixExpression)
	parser.registerPrefix(lexer.TRUE, parser.parseBoolean)
	parser.registerPrefix(lexer.FALSE, parser.parseBoolean)
	parser.registerPrefix(lexer.LPAREN, parser.parseGroupedExpression)

	// Register infix parsers.
	parser.infixParser = make(map[lexer.TokenType]infixParser)
	parser.registerInfix(lexer.PLUS, parser.parseInfixExpression)
	parser.registerInfix(lexer.MINUS, parser.parseInfixExpression)
	parser.registerInfix(lexer.SLASH, parser.parseInfixExpression)
	parser.registerInfix(lexer.ASTERISK, parser.parseInfixExpression)
	parser.registerInfix(lexer.POWER, parser.parseInfixExpression)
	parser.registerInfix(lexer.EQUAL, parser.parseInfixExpression)
	parser.registerInfix(lexer.NOTEQUAL, parser.parseInfixExpression)
	parser.registerInfix(lexer.LESS, parser.parseInfixExpression)
	parser.registerInfix(lexer.LESSEQ, parser.parseInfixExpression)
	parser.registerInfix(lexer.GREATER, parser.parseInfixExpression)
	parser.registerInfix(lexer.GREATEREQ, parser.parseInfixExpression)

	// Initialize the tokens.
	parser.getNextToken()
	parser.getNextToken()

	return parser
}

// ParseProgram parses the entire input into a program consisting of statements.
func (parser *Parser) ParseProgram() *Program {
	program := &Program{}
	program.Statements = []Statement{}

	// Parse statements until the end of the input.
	for parser.currentToken.Type != lexer.EOF {
		statement := parser.parseStatement()
		if statement != nil {
			program.Statements = append(program.Statements, statement)
		}
		parser.getNextToken()
	}

	return program
}

// Errors returns the list of parsing errors encountered during the parsing process.
func (parser *Parser) Errors() []string {
	return parser.errors
}

// nextTokenError adds an error message when the next token is not as expected.
func (parser *Parser) nextTokenError(tokenType lexer.TokenType) {
	message := fmt.Sprintf("expected next token to be %s, got %s instead", tokenType, parser.currentToken.Type)
	parser.errors = append(parser.errors, message)
}

// getNextToken advances to the next token from the lexer.
func (parser *Parser) getNextToken() {
	parser.currentToken = parser.nextToken
	parser.nextToken = parser.lexer_.NextToken()
}

// getNextPrecedence returns the precedence of the next token.
func (parser *Parser) getNextPrecedence() int {
	if precedence, ok := precedences[parser.nextToken.Type]; ok {
		return precedence
	}
	return LOWEST
}

// currentPrecedence returns the precedence of the current token.
func (parser *Parser) currentPrecedence() int {
	if precedence, ok := precedences[parser.currentToken.Type]; ok {
		return precedence
	}
	return LOWEST
}

// parseStatement parses a statement based on the current token type.
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

// registerPrefix registers a prefix parsing function for a given token type.
func (parser *Parser) registerPrefix(tokenType lexer.TokenType, prefix prefixParser) {
	parser.prefixParser[tokenType] = prefix
}

// registerInfix registers an infix parsing function for a given token type.
func (parser *Parser) registerInfix(tokenType lexer.TokenType, infix infixParser) {
	parser.infixParser[tokenType] = infix
}

// parseIdentifier parses an identifier as an expression.
func (parser *Parser) parseIdentifier() Expression {
	return &Identifier{Token: parser.currentToken, Value: parser.currentToken.Value}
}

// parseIntegerLiteral parses an integer literal as an expression.
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

// parseBoolean parses a boolean literal as an expression.
func (parser *Parser) parseBoolean() Expression {
	return &Boolean{Token: parser.currentToken, Value: parser.currentTokenIs(lexer.TRUE)}
}

// parseExpressionStatement parses an expression statement.
func (parser *Parser) parseExpressionStatement() *ExpressionStatement {
	statement := &ExpressionStatement{Token: parser.currentToken}
	statement.Expression = parser.parseExpression(LOWEST)

	if parser.nextTokenIs(lexer.SEMICOLON) {
		parser.getNextToken()
	}

	return statement
}

// parseExpression parses an expression with a given precedence.
func (parser *Parser) parseExpression(precedence int) Expression {
	prefix := parser.prefixParser[parser.currentToken.Type]
	if prefix == nil {
		parser.noPrefixParseFnError(parser.currentToken.Type)
		return nil
	}

	leftExpression := prefix()

	for !parser.nextTokenIs(lexer.SEMICOLON) && precedence < parser.getNextPrecedence() {
		infix := parser.infixParser[parser.nextToken.Type]
		if infix == nil {
			return leftExpression
		}

		parser.getNextToken()
		leftExpression = infix(leftExpression)
	}

	return leftExpression
}

// parsePrefixExpression parses a prefix expression.
func (parser *Parser) parsePrefixExpression() Expression {
	expression := &PrefixExpression{
		Token:    parser.currentToken,
		Operator: parser.currentToken.Value,
	}

	parser.getNextToken()
	expression.Right = parser.parseExpression(PREFIX)

	return expression
}

// parseInfixExpression parses an infix expression.
func (parser *Parser) parseInfixExpression(left Expression) Expression {
	expression := &InfixExpression{
		Token:    parser.currentToken,
		Operator: parser.currentToken.Value,
		Left:     left,
	}

	precedence := parser.currentPrecedence()
	parser.getNextToken()
	expression.Right = parser.parseExpression(precedence)

	return expression
}

// noPrefixParseFnError adds an error message when a prefix parsing function is not found for a token type.
func (parser *Parser) noPrefixParseFnError(tokenType lexer.TokenType) {
	message := fmt.Sprintf("no prefix parse function for %s found", tokenType)
	parser.errors = append(parser.errors, message)
}

// parseVarStatement parses a variable declaration statement.
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

// parseReturnStatement parses a return statement.
func (parser *Parser) parseReturnStatement() Statement {
	statement := &ReturnStatement{Token: parser.currentToken}

	for !parser.currentTokenIs(lexer.SEMICOLON) {
		parser.getNextToken()
	}

	return statement
}

// parseGroupedExpression parses an expression enclosed in parentheses.
func (parser *Parser) parseGroupedExpression() Expression {
	parser.getNextToken()
	expression := parser.parseExpression(LOWEST)

	if !parser.expectNext(lexer.RPAREN) {
		return nil
	}

	return expression
}

// expectNext checks if the next token matches the expected token type, advancing the token stream if it does.
func (parser *Parser) expectNext(tokenType lexer.TokenType) bool {
	if parser.nextTokenIs(tokenType) {
		parser.getNextToken()
		return true
	} else {
		parser.nextTokenError(tokenType)
		return false
	}
}

// currentTokenIs checks if the current token matches the given token type.
func (parser *Parser) currentTokenIs(tokenType lexer.TokenType) bool {
	return parser.currentToken.Type == tokenType
}

// nextTokenIs checks if the next token matches the given token type.
func (parser *Parser) nextTokenIs(tokenType lexer.TokenType) bool {
	return parser.nextToken.Type == tokenType
}
