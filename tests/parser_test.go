package tests

import (
	"Fungo/internal/lexer"
	"Fungo/internal/parser"
	"fmt"
	"testing"
)

func TestParserAst_VarStatement(t *testing.T) {
	input := `
	var first_var: int = 5;
	var second_var: int = 13;
	`
	program := getProgram(t, input)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if got := len(program.Statements); got != 2 {
		t.Fatalf("ParseProgram() did not return 2 statements. Got=%d", got)
	}

	tests := []struct {
		expectedIdentifier string
	}{
		{"first_var"},
		{"second_var"},
	}

	for i, tt := range tests {
		statement := program.Statements[i]
		if !testParserAstVarStatement(t, statement, tt.expectedIdentifier) {
			return
		}
	}
}

func TestParserAst_ReturnStatement(t *testing.T) {
	input := `
	return 5;
	return 13;
	`
	program := getProgram(t, input)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if got := len(program.Statements); got != 2 {
		t.Fatalf("ParseProgram() did not return 2 statements.\nGot: %d", got)
	}

	for _, statement := range program.Statements {
		retStatement, ok := statement.(*parser.ReturnStatement)
		if !ok {
			t.Errorf("ReturnStatement() did not return a ReturnStatement.\nGot: %T", statement)
			continue
		}

		if retStatement.TokenLiteral() != "return" {
			t.Errorf("ReturnStatement() did not return a 'return'.\nGot: %s", retStatement.TokenLiteral())
		}
	}
}

func TestParserAst_String(t *testing.T) {
	program := &parser.Program{
		Statements: []parser.Statement{
			&parser.VarStatement{
				Token: lexer.Token{Type: lexer.VARIABLE, Value: "var"},
				Name: &parser.Identifier{
					Token: lexer.Token{Type: lexer.IDENTIFIER, Value: "first_var"},
					Value: "first_var",
				},
				VarType: &parser.VarType{
					Token: lexer.Token{Type: lexer.IDENTIFIER},
					Value: "int",
				},
				Value: &parser.Identifier{
					Token: lexer.Token{Type: lexer.IDENTIFIER, Value: "second_var"},
					Value: "second_var",
				},
			},
		},
	}

	expected := "var first_var: int = second_var;"
	if program.String() != expected {
		t.Errorf("program.String() wrong.\nExpected: %q\nGot: %q", expected, program.String())
	}
}

func TestParserAst_IdentifierExpression(t *testing.T) {
	input := "some_var;"
	program := getProgram(t, input)

	if got := len(program.Statements); got != 1 {
		t.Fatalf("ParseProgram() did not return 1 statement.\nGot: %d", got)
	}

	statement, ok := program.Statements[0].(*parser.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not an ExpressionStatement.\nGot: %T", program.Statements[0])
	}

	identifier, ok := statement.Expression.(*parser.Identifier)
	if !ok {
		t.Fatalf("statement.Expression is not an Identifier.\nGot: %T", statement.Expression)
	}

	if identifier.Value != "some_var" {
		t.Errorf("identifier.Value=%q, expected 'some_var'", identifier.Value)
	}

	if identifier.TokenLiteral() != "some_var" {
		t.Errorf("identifier.TokenLiteral=%q, expected 'some_var'", identifier.TokenLiteral())
	}
}

func TestParserAst_IntegerLiteralExpression(t *testing.T) {
	input := "13;"
	program := getProgram(t, input)

	if got := len(program.Statements); got != 1 {
		t.Fatalf("ParseProgram() did not return 1 statement.\nGot: %d", got)
	}

	statement, ok := program.Statements[0].(*parser.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not an ExpressionStatement.\nGot: %T", program.Statements[0])
	}

	literal, ok := statement.Expression.(*parser.IntegerLiteral)
	if !ok {
		t.Fatalf("statement.Expression is not an IntegerLiteral.\nGot: %T", statement.Expression)
	}

	if literal.Value != 13 {
		t.Errorf("literal.Value=%d, expected 13", literal.Value)
	}

	if literal.TokenLiteral() != "13" {
		t.Errorf("literal.TokenLiteral=%q, expected '13'", literal.TokenLiteral())
	}
}

func TestParserAst_PrefixExpression(t *testing.T) {
	prefixTests := []struct {
		input    string
		operator string
		value    int64
	}{
		{"!13;", "!", 13},
		{"-13;", "-", 13},
	}

	for _, tt := range prefixTests {
		program := getProgram(t, tt.input)

		if got := len(program.Statements); got != 1 {
			t.Fatalf("ParseProgram() did not return 1 statement.\nGot: %d", got)
		}

		statement, ok := program.Statements[0].(*parser.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not an ExpressionStatement.\nGot: %T", program.Statements[0])
		}

		expression, ok := statement.Expression.(*parser.PrefixExpression)
		if !ok {
			t.Fatalf("statement.Expression is not a PrefixExpression.\nGot: %T", statement.Expression)
		}

		if expression.Operator != tt.operator {
			t.Fatalf("expression.Operator is not '%s'.\nGot: '%s'", tt.operator, expression.Operator)
		}

		if !testIntegerValue(t, expression.Right, tt.value) {
			return
		}
	}
}

func TestParserAst_InfixExpression(t *testing.T) {
	infixTests := []struct {
		input      string
		leftValue  int64
		operator   string
		rightValue int64
	}{
		{"5 + 5;", 5, "+", 5},
		{"5 - 5;", 5, "-", 5},
		{"5 * 5;", 5, "*", 5},
		{"5 ** 5;", 5, "**", 5},
		{"5 / 5;", 5, "/", 5},
		{"5 > 5;", 5, ">", 5},
		{"5 >= 5;", 5, ">=", 5},
		{"5 < 5;", 5, "<", 5},
		{"5 <= 5;", 5, "<=", 5},
		{"5 == 5;", 5, "==", 5},
		{"5 != 5;", 5, "!=", 5},
	}

	for _, tt := range infixTests {
		program := getProgram(t, tt.input)

		if got := len(program.Statements); got != 1 {
			t.Fatalf("ParseProgram() did not return 1 statement.\nGot: %d", got)
		}

		statement, ok := program.Statements[0].(*parser.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not an ExpressionStatement.\nGot: %T", program.Statements[0])
		}

		expression, ok := statement.Expression.(*parser.InfixExpression)
		if !ok {
			t.Fatalf("statement.Expression is not an InfixExpression.\nGot: %T", statement.Expression)
		}

		if !testIntegerValue(t, expression.Left, tt.leftValue) {
			return
		}

		if expression.Operator != tt.operator {
			t.Fatalf("exp.Operator is not '%s'.\nGot: %s", tt.operator, expression.Operator)
		}

		if !testIntegerValue(t, expression.Right, tt.rightValue) {
			return
		}
	}
}

func testParserAstVarStatement(t *testing.T, statement parser.Statement, tokenName string) bool {
	t.Helper()
	if statement.TokenLiteral() != "var" {
		t.Errorf("Token literal should be \"var\".\nGot: %s", statement.TokenLiteral())
		return false
	}

	varStatement, ok := statement.(*parser.VarStatement)
	if !ok {
		t.Errorf("Statement has wrong type: %T", statement)
		return false
	}

	if varStatement.Name.Value != tokenName {
		t.Errorf("varStatement.Name.Value is not '%s'.\nGot: %s", tokenName, varStatement.Name.Value)
		return false
	}

	if varStatement.Name.TokenLiteral() != tokenName {
		t.Errorf("varStatement.Name.TokenLiteral is not '%s'.\nGot: %s", tokenName, varStatement.Name.TokenLiteral())
		return false
	}

	return true
}

func testIntegerValue(t *testing.T, expression parser.Expression, value int64) bool {
	t.Helper()
	integer, ok := expression.(*parser.IntegerLiteral)

	if !ok {
		t.Errorf("expression is not int value")
		return false
	}

	if integer.Value != value {
		t.Errorf("Expected integer.Value=%d, got %d", value, integer.Value)
		return false
	}

	if integer.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("Expected TokenLiteral=%d, got %s", value, integer.TokenLiteral())
		return false
	}

	return true
}

func testIdentifier(t *testing.T, expression parser.Expression, value string) bool {
	t.Helper()
	identifier, ok := expression.(*parser.Identifier)
	if !ok {
		t.Errorf("expression is not identifier")
		return false
	}

	if identifier.Value != value {
		t.Errorf("Expected identifier.Value='%s', got %s", value, identifier.Value)
		return false
	}

	if identifier.TokenLiteral() != value {
		t.Errorf("Expected TokenLiteral='%s', got %s", value, identifier.TokenLiteral())
		return false
	}

	return true
}

func getProgram(t *testing.T, input string) *parser.Program {
	t.Helper()
	lexer_ := lexer.New(input)
	parser_ := parser.New(lexer_)
	program := parser_.ParseProgram()
	checkParserErrors(t, parser_)
	return program
}

func checkParserErrors(t *testing.T, parser_ *parser.Parser) {
	t.Helper()
	errors := parser_.Errors()
	if len(errors) != 0 {
		t.Fatalf("Parser has errors: %v", errors)
	}
}

func testLiteralExpression(t *testing.T, expression parser.Expression, expected interface{}) bool {
	t.Helper()
	switch v := expected.(type) {
	case int, int64:
		return testIntegerValue(t, expression, v.(int64))
	case string:
		return testIdentifier(t, expression, v)
	}
	t.Errorf("Unsupported type for expression: %T", expected)
	return false
}
