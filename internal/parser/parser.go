package parser

import (
	"Fungo/internal/lexer"
	"Fungo/internal/lexer/token"
	"Fungo/internal/parser/ast"
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
var precedences = map[token.Type]int{
	token.EQUAL:     EQUALS,
	token.NOTEQUAL:  EQUALS,
	token.LESS:      LESSGREATER,
	token.GREATER:   LESSGREATER,
	token.LESSEQ:    LESSGREATEREQ,
	token.GREATEREQ: LESSGREATEREQ,
	token.PLUS:      SUM,
	token.MINUS:     SUM,
	token.SLASH:     PRODUCT,
	token.ASTERISK:  PRODUCT,
	token.POWER:     EXPONENT,
}

type (
	// prefixParser defines a function type that parses prefix expressions.
	prefixParser func() ast.Expression

	// infixParser defines a function type that parses infix expressions.
	infixParser func(ast.Expression) ast.Expression
)

// Parser represents the main structure for parsing tokens from the lexer and building an AST.
type Parser struct {
	lexer_       *lexer.Lexer                // lexer_ is the lexer that provides the tokens.
	currentToken token.Token                 // currentToken is the current token being parsed.
	nextToken    token.Token                 // nextToken is the next token to be parsed.
	errors       []string                    // errors stores a list of parsing errors.
	prefixParser map[token.Type]prefixParser // prefixParser maps token types to their prefix parsers.
	infixParser  map[token.Type]infixParser  // infixParser maps token types to their infix parsers.
}

// New creates a new parser instance with a given lexer and initializes the prefix and infix parsers.
func New(lexer_ *lexer.Lexer) *Parser {
	parser := &Parser{
		lexer_: lexer_,
		errors: []string{},
	}

	// Register prefix parsers.
	parser.prefixParser = make(map[token.Type]prefixParser)
	parser.registerPrefix(token.IDENTIFIER, parser.parseIdentifier)
	parser.registerPrefix(token.INTEGER, parser.parseIntegerLiteral)
	parser.registerPrefix(token.NEGATION, parser.parsePrefixExpression)
	parser.registerPrefix(token.MINUS, parser.parsePrefixExpression)
	parser.registerPrefix(token.TRUE, parser.parseBoolean)
	parser.registerPrefix(token.FALSE, parser.parseBoolean)
	parser.registerPrefix(token.LPAREN, parser.parseGroupedExpression)
	parser.registerPrefix(token.IF, parser.parseIfExpression)

	// Register infix parsers.
	parser.infixParser = make(map[token.Type]infixParser)
	parser.registerInfix(token.PLUS, parser.parseInfixExpression)
	parser.registerInfix(token.MINUS, parser.parseInfixExpression)
	parser.registerInfix(token.SLASH, parser.parseInfixExpression)
	parser.registerInfix(token.ASTERISK, parser.parseInfixExpression)
	parser.registerInfix(token.POWER, parser.parseInfixExpression)
	parser.registerInfix(token.EQUAL, parser.parseInfixExpression)
	parser.registerInfix(token.NOTEQUAL, parser.parseInfixExpression)
	parser.registerInfix(token.LESS, parser.parseInfixExpression)
	parser.registerInfix(token.LESSEQ, parser.parseInfixExpression)
	parser.registerInfix(token.GREATER, parser.parseInfixExpression)
	parser.registerInfix(token.GREATEREQ, parser.parseInfixExpression)

	// Initialize the tokens.
	parser.getNextToken()
	parser.getNextToken()

	return parser
}

// ParseProgram parses the entire input into a program consisting of statements.
func (parser *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	// Parse statements until the end of the input.
	for parser.currentToken.Type != token.EOF {
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
func (parser *Parser) nextTokenError(tokenType token.Type) {
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
func (parser *Parser) parseStatement() ast.Statement {
	switch parser.currentToken.Type {
	case token.VARIABLE:
		return parser.parseVarStatement()
	case token.RETURN:
		return parser.parseReturnStatement()
	default:
		return parser.parseExpressionStatement()
	}
}

// registerPrefix registers a prefix parsing function for a given token type.
func (parser *Parser) registerPrefix(tokenType token.Type, prefix prefixParser) {
	parser.prefixParser[tokenType] = prefix
}

// registerInfix registers an infix parsing function for a given token type.
func (parser *Parser) registerInfix(tokenType token.Type, infix infixParser) {
	parser.infixParser[tokenType] = infix
}

// parseIdentifier parses an identifier as an expression.
func (parser *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: parser.currentToken, Value: parser.currentToken.Value}
}

// parseIntegerLiteral parses an integer literal as an expression.
func (parser *Parser) parseIntegerLiteral() ast.Expression {
	integerLiteral := &ast.IntegerLiteral{Token: parser.currentToken}

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
func (parser *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{Token: parser.currentToken, Value: parser.currentTokenIs(token.TRUE)}
}

// parseExpressionStatement parses an expression statement.
func (parser *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	statement := &ast.ExpressionStatement{Token: parser.currentToken}
	statement.Expression = parser.parseExpression(LOWEST)

	if parser.nextTokenIs(token.SEMICOLON) {
		parser.getNextToken()
	}

	return statement
}

// parseExpression parses an expression with a given precedence.
func (parser *Parser) parseExpression(precedence int) ast.Expression {
	prefix := parser.prefixParser[parser.currentToken.Type]
	if prefix == nil {
		parser.noPrefixParseFnError(parser.currentToken.Type)
		return nil
	}

	leftExpression := prefix()

	for !parser.nextTokenIs(token.SEMICOLON) && precedence < parser.getNextPrecedence() {
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
func (parser *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    parser.currentToken,
		Operator: parser.currentToken.Value,
	}

	parser.getNextToken()
	expression.Right = parser.parseExpression(PREFIX)

	return expression
}

// parseInfixExpression parses an infix expression.
func (parser *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
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
func (parser *Parser) noPrefixParseFnError(tokenType token.Type) {
	message := fmt.Sprintf("no prefix parse function for %s found", tokenType)
	parser.errors = append(parser.errors, message)
}

// parseVarStatement parses a variable declaration statement.
func (parser *Parser) parseVarStatement() ast.Statement {
	statement := &ast.VarStatement{Token: parser.currentToken}

	if !parser.expectNext(token.IDENTIFIER) {
		return nil
	}

	statement.Name = &ast.Identifier{Token: parser.currentToken, Value: parser.currentToken.Value}

	if !parser.expectNext(token.COLON) {
		return nil
	}
	if !parser.expectNext(token.VARTYPE) {
		return nil
	}

	statement.VarType = &ast.VarType{Token: parser.currentToken, Value: parser.currentToken.Value}

	if !parser.expectNext(token.ASSIGN) {
		return nil
	}

	for !parser.currentTokenIs(token.SEMICOLON) {
		parser.getNextToken()
	}

	return statement
}

// parseReturnStatement parses a return statement.
func (parser *Parser) parseReturnStatement() ast.Statement {
	statement := &ast.ReturnStatement{Token: parser.currentToken}

	for !parser.currentTokenIs(token.SEMICOLON) {
		parser.getNextToken()
	}

	return statement
}

// parseBlockStatement parses a block of statements
// for example in if expressions
func (parser *Parser) parseBlockStatement() *ast.BlockStatement {
	block := &ast.BlockStatement{Token: parser.currentToken}
	block.Statements = make([]ast.Statement, 0)

	parser.getNextToken()

	for !parser.currentTokenIs(token.RBRACE) && !parser.currentTokenIs(token.EOF) {
		statement := parser.parseStatement()
		if statement != nil {
			block.Statements = append(block.Statements, statement)
		}
		parser.getNextToken()
	}

	return block
}

// parseGroupedExpression parses an expression enclosed in parentheses.
func (parser *Parser) parseGroupedExpression() ast.Expression {
	parser.getNextToken()
	expression := parser.parseExpression(LOWEST)

	if !parser.expectNext(token.RPAREN) {
		return nil
	}

	return expression
}

// parseIfExpression parses an if expression from the input tokens.
// It returns an AST representation of the if expression or nil if parsing fails.
func (parser *Parser) parseIfExpression() ast.Expression {
	expression := &ast.IFExpression{Token: parser.currentToken}

	if !parser.expectNext(token.LPAREN) {
		return nil
	}

	parser.getNextToken()
	expression.Condition = parser.parseExpression(LOWEST)

	if !parser.expectNext(token.RPAREN) || !parser.expectNext(token.LBRACE) {
		return nil
	}

	expression.Consequence = parser.parseBlockStatement()

	if parser.nextTokenIs(token.ELSE) {
		parser.getNextToken()

		if !parser.expectNext(token.LBRACE) {
			return nil
		}

		expression.Alternative = parser.parseBlockStatement()
	}

	return expression

}

// expectNext checks if the next token matches the expected token type, advancing the token stream if it does.
func (parser *Parser) expectNext(tokenType token.Type) bool {
	if parser.nextTokenIs(tokenType) {
		parser.getNextToken()
		return true
	} else {
		parser.nextTokenError(tokenType)
		return false
	}
}

// currentTokenIs checks if the current token matches the given token type.
func (parser *Parser) currentTokenIs(tokenType token.Type) bool {
	return parser.currentToken.Type == tokenType
}

// nextTokenIs checks if the next token matches the given token type.
func (parser *Parser) nextTokenIs(tokenType token.Type) bool {
	return parser.nextToken.Type == tokenType
}
