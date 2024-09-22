package tests

import (
	"Fungo/internal/lexer/token"
	"Fungo/internal/parser/ast"
	"testing"
)

// TestParserAst_VarStatement tests the parsing of variable declarations.
func TestParserAst_VarStatement(t *testing.T) {
	input := `
	var first_var: int = 5;
	var second_var: int = 13;
	`
	program := getProgram(t, input)

	// Verify that the program is correctly parsed.
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	// Check if the program contains exactly 2 statements.
	if got := len(program.Statements); got != 2 {
		t.Fatalf("ParseProgram() did not return 2 statements. Got=%d", got)
	}

	// Define expected variable identifiers for comparison.
	tests := []struct {
		expectedIdentifier string
	}{
		{"first_var"},
		{"second_var"},
	}

	// Check each statement against expected variable names.
	for i, tt := range tests {
		statement := program.Statements[i]
		if !testVarStatement(t, statement, tt.expectedIdentifier) {
			return
		}
	}
}

// TestParserAst_ReturnStatement tests the parsing of return statements.
func TestParserAst_ReturnStatement(t *testing.T) {
	input := `
	return 5;
	return 13;
	`
	program := getProgram(t, input)

	// Verify that the program is correctly parsed.
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	// Check if the program contains exactly 2 statements.
	if got := len(program.Statements); got != 2 {
		t.Fatalf("ParseProgram() did not return 2 statements.\nGot: %d", got)
	}

	// Check each return statement.
	for _, statement := range program.Statements {
		retStatement, ok := statement.(*ast.ReturnStatement)
		if !ok {
			t.Errorf("ReturnStatement() did not return a ReturnStatement.\nGot: %T", statement)
			continue
		}

		// Verify that the token is correctly identified as 'return'.
		if retStatement.TokenLiteral() != "return" {
			t.Errorf("ReturnStatement() did not return a 'return'.\nGot: %s", retStatement.TokenLiteral())
		}
	}
}

// TestParserAst_String tests the conversion of AST to string representation.
func TestParserAst_String(t *testing.T) {
	program := &ast.Program{
		Statements: []ast.Statement{
			&ast.VarStatement{
				Token: token.Token{Type: token.VARIABLE, Value: "var"},
				Name: &ast.Identifier{
					Token: token.Token{Type: token.IDENTIFIER, Value: "first_var"},
					Value: "first_var",
				},
				VarType: &ast.VarType{
					Token: token.Token{Type: token.IDENTIFIER},
					Value: "int",
				},
				Value: &ast.Identifier{
					Token: token.Token{Type: token.IDENTIFIER, Value: "second_var"},
					Value: "second_var",
				},
			},
		},
	}

	// Expected string representation of the program.
	expected := "var first_var: int = second_var;"
	if program.String() != expected {
		t.Errorf("program.String() wrong.\nExpected: %q\nGot: %q", expected, program.String())
	}
}

// TestParserAst_IdentifierExpression tests the parsing of an identifier expression.
func TestParserAst_IdentifierExpression(t *testing.T) {
	input := "some_var;"
	program := getProgram(t, input)

	// Check if there is exactly 1 statement.
	if got := len(program.Statements); got != 1 {
		t.Fatalf("ParseProgram() did not return 1 statement.\nGot: %d", got)
	}

	// Ensure the statement is an expression statement.
	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not an ExpressionStatement.\nGot: %T", program.Statements[0])
	}

	// Verify that the expression is an identifier.
	identifier, ok := statement.Expression.(*ast.Identifier)
	if !ok {
		t.Fatalf("statement.Expression is not an Identifier.\nGot: %T", statement.Expression)
	}

	// Check the identifier's value and literal.
	if identifier.Value != "some_var" {
		t.Errorf("identifier.Value=%q, expected 'some_var'", identifier.Value)
	}

	if identifier.TokenLiteral() != "some_var" {
		t.Errorf("identifier.TokenLiteral=%q, expected 'some_var'", identifier.TokenLiteral())
	}
}

// TestParserAst_IntegerLiteralExpression tests the parsing of an integer literal.
func TestParserAst_IntegerLiteralExpression(t *testing.T) {
	input := "13;"
	program := getProgram(t, input)

	// Check if there is exactly 1 statement.
	if got := len(program.Statements); got != 1 {
		t.Fatalf("ParseProgram() did not return 1 statement.\nGot: %d", got)
	}

	// Ensure the statement is an expression statement.
	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not an ExpressionStatement.\nGot: %T", program.Statements[0])
	}

	// Verify that the expression is an integer literal.
	literal, ok := statement.Expression.(*ast.IntegerLiteral)
	if !ok {
		t.Fatalf("statement.Expression is not an IntegerLiteral.\nGot: %T", statement.Expression)
	}

	// Check the literal's value and token literal.
	if literal.Value != 13 {
		t.Errorf("literal.Value=%d, expected 13", literal.Value)
	}

	if literal.TokenLiteral() != "13" {
		t.Errorf("literal.TokenLiteral=%q, expected '13'", literal.TokenLiteral())
	}
}

// TestParserAst_PrefixExpression tests the parsing of prefix expressions (e.g., -5, !true).
func TestParserAst_PrefixExpression(t *testing.T) {
	prefixTests := []struct {
		input    string
		operator string
		value    int64
	}{
		{"!13;", "!", 13},
		{"-13;", "-", 13},
	}

	// Test each prefix expression.
	for _, tt := range prefixTests {
		program := getProgram(t, tt.input)

		// Check if there is exactly 1 statement.
		if got := len(program.Statements); got != 1 {
			t.Fatalf("ParseProgram() did not return 1 statement.\nGot: %d", got)
		}

		// Ensure the statement is an expression statement.
		statement, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not an ExpressionStatement.\nGot: %T", program.Statements[0])
		}

		// Verify the prefix expression.
		expression, ok := statement.Expression.(*ast.PrefixExpression)
		if !ok {
			t.Fatalf("statement.Expression is not a PrefixExpression.\nGot: %T", statement.Expression)
		}

		if expression.Operator != tt.operator {
			t.Fatalf("expression.Operator is not '%s'.\nGot: '%s'", tt.operator, expression.Operator)
		}

		if !testIntegerLiteral(t, expression.Right, tt.value) {
			return
		}
	}
}

// TestParserAst_InfixExpression tests the parsing of infix expressions (e.g., 5 + 5).
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

	// Test each infix expression.
	for _, tt := range infixTests {
		program := getProgram(t, tt.input)

		// Check if there is exactly 1 statement.
		if got := len(program.Statements); got != 1 {
			t.Fatalf("ParseProgram() did not return 1 statement.\nGot: %d", got)
		}

		// Ensure the statement is an expression statement.
		statement, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not an ExpressionStatement.\nGot: %T", program.Statements[0])
		}

		// Verify the infix expression.
		expression, ok := statement.Expression.(*ast.InfixExpression)
		if !ok {
			t.Fatalf("statement.Expression is not an InfixExpression.\nGot: %T", statement.Expression)
		}

		// Test left and right values, and operator.
		if !testIntegerLiteral(t, expression.Left, tt.leftValue) {
			return
		}

		if expression.Operator != tt.operator {
			t.Fatalf("exp.Operator is not '%s'.\nGot: %s", tt.operator, expression.Operator)
		}

		if !testIntegerLiteral(t, expression.Right, tt.rightValue) {
			return
		}
	}
}

// TestParserAST_IfExpression tests the parsing of if-else expressions.
func TestParserAST_IfExpression(t *testing.T) {
	input := `
	if (x < y) {
		x
	} else {
		y
	}`

	// Parse the program and check for exactly one statement.
	program := getProgram(t, input)
	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. Got=%d", len(program.Statements))
	}

	// Ensure the statement is an ExpressionStatement.
	statement, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not an ExpressionStatement. Got=%T", program.Statements[0])
	}

	// Verify the expression is an IFExpression.
	ifExpression, ok := statement.Expression.(*ast.IFExpression)
	if !ok {
		t.Fatalf("statement.Expression is not an IFExpression. Got=%T", statement.Expression)
	}

	// Verify the condition.
	if !testInfixExpression(t, ifExpression.Condition, "x", "<", "y") {
		return
	}

	// Verify the consequence block.
	if len(ifExpression.Consequence.Statements) != 1 {
		t.Fatalf("Consequence block does not contain 1 statement. Got=%d", len(ifExpression.Consequence.Statements))
	}

	consequenceStatement, ok := ifExpression.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Consequence statement is not an ExpressionStatement. Got=%T", ifExpression.Consequence.Statements[0])
	}

	// Check if the consequence expression is correct.
	testIdentifier(t, consequenceStatement.Expression, "x")

	// Verify the alternative block.
	if ifExpression.Alternative == nil {
		t.Fatalf("Alternative block is nil.")
	}

	if len(ifExpression.Alternative.Statements) != 1 {
		t.Fatalf("Alternative block does not contain 1 statement. Got=%d", len(ifExpression.Alternative.Statements))
	}

	alternativeStatement, ok := ifExpression.Alternative.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Alternative statement is not an ExpressionStatement. Got=%T", ifExpression.Alternative.Statements[0])
	}

	// Check if the alternative expression is correct.
	testIdentifier(t, alternativeStatement.Expression, "y")
}
