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
	lexer_ := lexer.New(input)
	parser_ := parser.New(lexer_)

	program := parser_.ParseProgram()
	checkParserErrors(t, parser_)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 2 {
		t.Fatalf("ParseProgram() did not return 2 statements. Got=%d", len(program.Statements))
	}

	tests := []struct {
		expectedIdentifier string
	}{
		{"first_var"},
		{"second_var"},
	}

	for iter, tokenType := range tests {
		statement := program.Statements[iter]
		if !testParserAstVarStatement(t, statement, tokenType.expectedIdentifier) {
			return
		}
	}
}

func TestParserAst_ReturnStatement(t *testing.T) {
	input := `
	return 5;
	return 13;
	`
	lexer_ := lexer.New(input)
	parser_ := parser.New(lexer_)

	program := parser_.ParseProgram()
	checkParserErrors(t, parser_)

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}

	if len(program.Statements) != 2 {
		t.Fatalf("ParseProgram() did not return 2 statements. Got=%d", len(program.Statements))
	}

	for _, statement := range program.Statements {
		retStatement, ok := statement.(*parser.ReturnStatement)

		if !ok {
			t.Errorf("ReturnStatement() did not return a ReturnStatement. Got=%T", statement)
			continue
		}

		if retStatement.TokenLiteral() != "return" {
			t.Errorf("ReturnStatement() did not return a return. Got=%s", retStatement.TokenLiteral())
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

	if program.String() != "var first_var: int = second_var;" {
		t.Errorf("program.String() wrong. got=%q", program.String())
	}
}

func TestParserAst_IdentifierExpression(t *testing.T) {
	input := "some_var;"

	lexer_ := lexer.New(input)
	parser_ := parser.New(lexer_)
	program := parser_.ParseProgram()
	checkParserErrors(t, parser_)

	if len(program.Statements) != 1 {
		t.Fatalf("ParseProgram() did not return 1 statements. Got=%d", len(program.Statements))
	}

	statement, ok := program.Statements[0].(*parser.ExpressionStatement)

	if !ok {
		t.Errorf("program.Statements[0] is not a ExpressionStatement. Got=%T", program.Statements[0])
	}

	identifier, ok := statement.Expression.(*parser.Identifier)

	if !ok {
		t.Errorf("statement.Expression is not a Identifier. Got=%T", statement.Expression)
	}

	if identifier.Value != "some_var" {
		t.Errorf("identifier.Value=%q", identifier.Value)
	}

	if identifier.TokenLiteral() != "some_var" {
		t.Errorf("identifier.TokenLiteral=%q", identifier.TokenLiteral())
	}
}

func TestParserAst_IntegerLiteralExpression(t *testing.T) {
	input := "13;"

	lexer_ := lexer.New(input)
	parser_ := parser.New(lexer_)
	program := parser_.ParseProgram()
	checkParserErrors(t, parser_)

	if len(program.Statements) != 1 {
		t.Fatalf("ParseProgram() did not return 1 statements. Got=%d", len(program.Statements))
	}

	statement, ok := program.Statements[0].(*parser.ExpressionStatement)

	if !ok {
		t.Errorf("program.Statements[0] is not a ExpressionStatement. Got=%T", program.Statements[0])
	}

	statementValue, ok := statement.Expression.(*parser.IntegerLiteral)

	if !ok {
		t.Errorf("statement.Expression is not a IntegerLiteral. Got=%T", statement.Expression)
	}

	if statementValue.Value != 13 {
		t.Errorf("statement_value.Value=%d", statementValue.Value)
	}

	if statement.TokenLiteral() != "13" {
		t.Errorf("statement.TokenLiteral=%q", statement.TokenLiteral())
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
		lexer_ := lexer.New(tt.input)
		parser_ := parser.New(lexer_)
		program := parser_.ParseProgram()
		checkParserErrors(t, parser_)

		if len(program.Statements) != 1 {
			t.Fatalf("ParseProgram() did not return 1 statements. Got=%d", len(program.Statements))
		}

		statement, ok := program.Statements[0].(*parser.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not a ExpressionStatement. Got=%T", program.Statements[0])
		}

		expression, ok := statement.Expression.(*parser.PrefixExpression)
		if !ok {
			t.Fatalf("statement.Expression is not a PrefixExpression. Got=%T", statement.Expression)
		}

		if expression.Operator != tt.operator {
			t.Fatalf("expression.Operator is not '%s'. got '%s'", tt.operator, expression.Operator)
		}

		if !testIntegerValue(t, expression.Right, tt.value) {
			return
		}
	}
}

func testParserAstVarStatement(t *testing.T, statement parser.Statement, tokenName string) bool {
	if statement.TokenLiteral() != "var" {
		t.Errorf("Token literal should be \"var\" but was: %s", statement.TokenLiteral())
		return false
	}

	varStatement, ok := statement.(*parser.VarStatement)
	if !ok {
		t.Errorf("Statement has wrong type: %T", varStatement)
		return false
	}

	if varStatement.Name.Value != tokenName {
		t.Errorf("varStatement.Name.Value is not '%s'. got=%s", tokenName, varStatement.Name.Value)
		return false
	}

	if varStatement.Name.TokenLiteral() != tokenName {
		t.Errorf("s.Name is not '%s'. got=%s", tokenName, varStatement.Name)
		return false
	}

	return true
}

func testIntegerValue(t *testing.T, expression parser.Expression, value int64) bool {
	integer, ok := expression.(*parser.IntegerLiteral)

	if !ok {
		t.Errorf("expression is not int value")
		return false
	}

	if integer.Value != value {
		t.Errorf("incorrect value")
		return false
	}

	if integer.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("integer.TokenLiteral not %d. got=%s", value, integer.TokenLiteral())
		return false
	}

	return true
}

func checkParserErrors(t *testing.T, parser_ *parser.Parser) {
	errors := parser_.Errors()

	if len(errors) == 0 {
		return
	}

	t.Errorf("Parser has %d errors", len(errors))

	for _, message := range errors {
		t.Errorf("Parser error: %s", message)
	}

	t.FailNow()
}
