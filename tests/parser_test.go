package tests

import (
	"Fungo/internal/lexer"
	"Fungo/internal/parser"
	"testing"
)

func TestParserAst_VarStatement(t *testing.T) {
	input := `
var x = 5;
var y = 13;
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
		{"x"},
		{"y"},
	}

	for iter, tokenType := range tests {
		statement := program.Statements[iter]
		if !testParserAstVarStatement(t, statement, tokenType.expectedIdentifier) {
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
