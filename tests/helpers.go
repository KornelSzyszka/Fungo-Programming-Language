package tests

import (
	"Fungo/internal/lexer"
	"Fungo/internal/parser"
	"Fungo/internal/parser/ast"
	"fmt"
	"testing"
)

// testLiteralExpression checks if an expression matches the expected literal value
// It handles int, int64, bool, and string types generically.
func testLiteralExpression(t *testing.T, expression ast.Expression, expected interface{}) bool {
	t.Helper()

	switch v := expected.(type) {
	case int:
		return testIntegerLiteral(t, expression, int64(v))
	case int64:
		return testIntegerLiteral(t, expression, v)
	case string:
		return testIdentifier(t, expression, v)
	case bool:
		return testBooleanLiteral(t, expression, v)
	default:
		t.Errorf("Unexpected literal type: %T", expected)
		return false
	}
}

// testInfixExpression tests whether an expression is a valid InfixExpression
// with the provided left value, operator, and right value.
func testInfixExpression(t *testing.T, expression ast.Expression, left interface{}, operator string, right interface{}) bool {
	t.Helper()

	infixExpression, ok := expression.(*ast.InfixExpression)
	if !ok {
		t.Errorf("Expected InfixExpression\nGot: %T", expression)
		return false
	}

	if !testLiteralExpression(t, infixExpression.Left, left) {
		return false
	}

	if infixExpression.Operator != operator {
		t.Errorf("Expected operator: '%s'\nGot: %s", operator, infixExpression.Operator)
		return false
	}

	if !testLiteralExpression(t, infixExpression.Right, right) {
		return false
	}

	return true
}

// testIntegerLiteral checks if the expression is an IntegerLiteral and matches the expected value.
func testIntegerLiteral(t *testing.T, expression ast.Expression, value int64) bool {
	t.Helper()

	integerLiteral, ok := expression.(*ast.IntegerLiteral)
	if !ok {
		t.Errorf("Expected IntegerLiteral\nGot: %T", expression)
		return false
	}

	if integerLiteral.Value != value {
		t.Errorf("Expected IntegerLiteral value: %d\nGot: %d", value, integerLiteral.Value)
		return false
	}

	if integerLiteral.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("Expected IntegerLiteral TokenLiteral: '%d'\nGot: %s", value, integerLiteral.TokenLiteral())
		return false
	}

	return true
}

// testIdentifier checks if the expression is an Identifier and matches the expected value.
func testIdentifier(t *testing.T, expression ast.Expression, value string) bool {
	t.Helper()

	identifier, ok := expression.(*ast.Identifier)
	if !ok {
		t.Errorf("Expected Identifier\nGot: %T", expression)
		return false
	}

	if identifier.Value != value {
		t.Errorf("Expected identifier.Value: '%s'\nGot: %s", value, identifier.Value)
		return false
	}

	if identifier.TokenLiteral() != value {
		t.Errorf("Expected TokenLiteral: '%s'\nGot: %s", value, identifier.TokenLiteral())
		return false
	}

	return true
}

// testBooleanLiteral checks if the expression is a BooleanLiteral and matches the expected value.
func testBooleanLiteral(t *testing.T, expression ast.Expression, value bool) bool {
	t.Helper()

	booleanLiteral, ok := expression.(*ast.Boolean)
	if !ok {
		t.Errorf("Expected BooleanLiteral\nGot: %T", expression)
		return false
	}

	if booleanLiteral.Value != value {
		t.Errorf("Expected BooleanLiteral value: %t\nGot: %t", value, booleanLiteral.Value)
		return false
	}

	if booleanLiteral.TokenLiteral() != fmt.Sprintf("%t", value) {
		t.Errorf("Expected BooleanLiteral TokenLiteral: '%t'\nGot: %s", value, booleanLiteral.TokenLiteral())
		return false
	}

	return true
}

// getProgram parses the input string and returns the resulting AST program.
func getProgram(t *testing.T, input string) *ast.Program {
	t.Helper()

	lexer_ := lexer.New(input)
	parser_ := parser.New(lexer_)
	program := parser_.ParseProgram()
	checkParserErrors(t, parser_)
	return program
}

// checkParserErrors reports any parsing errors encountered.
func checkParserErrors(t *testing.T, parser_ *parser.Parser) {
	t.Helper()

	errors := parser_.Errors()
	if len(errors) != 0 {
		t.Fatalf("Parser encountered errors: %v", errors)
	}
}

// testVarStatement checks if the provided statement is a valid VarStatement with the given name.
func testVarStatement(t *testing.T, statement ast.Statement, name string) bool {
	t.Helper()

	varStatement, ok := statement.(*ast.VarStatement)
	if !ok {
		t.Errorf("Expected VarStatement\nGot: %T", statement)
		return false
	}

	if varStatement.Name.Value != name {
		t.Errorf("Expected varStatement.Name.Value: '%s'\nGot: %s", name, varStatement.Name.Value)
		return false
	}

	if varStatement.Name.TokenLiteral() != name {
		t.Errorf("Expected varStatement.Name.TokenLiteral: '%s'\nGot: %s", name, varStatement.Name.TokenLiteral())
		return false
	}

	return true
}
