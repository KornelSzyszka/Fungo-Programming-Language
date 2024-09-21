package tests

import (
	"Fungo/internal/lexer"
	"testing"
)

func TestLexer_NextToken(t *testing.T) {
	input := `
	var five: int = 5;
	var ten: int = 10;
	
	var sum: int = func add(x: int , y: int) -> int {
		return x + y;
	}

	sum++;

	if (sum >= 13) {
		return sum;
	}
	
	return 0;
	`

	tests := []struct {
		expectedType  lexer.TokenType
		expectedValue string
	}{
		{lexer.VARIABLE, "var"},
		{lexer.IDENTIFIER, "five"},
		{lexer.COLON, ":"},
		{lexer.VARTYPE, "int"},
		{lexer.ASSIGN, "="},
		{lexer.INTEGER, "5"},
		{lexer.SEMICOLON, ";"},
		{lexer.VARIABLE, "var"},
		{lexer.IDENTIFIER, "ten"},
		{lexer.COLON, ":"},
		{lexer.VARTYPE, "int"},
		{lexer.ASSIGN, "="},
		{lexer.INTEGER, "10"},
		{lexer.SEMICOLON, ";"},
		{lexer.VARIABLE, "var"},
		{lexer.IDENTIFIER, "sum"},
		{lexer.COLON, ":"},
		{lexer.VARTYPE, "int"},
		{lexer.ASSIGN, "="},
		{lexer.FUNCTION, "func"},
		{lexer.IDENTIFIER, "add"},
		{lexer.LPAREN, "("},
		{lexer.IDENTIFIER, "x"},
		{lexer.COLON, ":"},
		{lexer.VARTYPE, "int"},
		{lexer.COMMA, ","},
		{lexer.IDENTIFIER, "y"},
		{lexer.COLON, ":"},
		{lexer.VARTYPE, "int"},
		{lexer.RPAREN, ")"},
		{lexer.ARROW, "->"},
		{lexer.VARTYPE, "int"},
		{lexer.LBRACE, "{"},
		{lexer.RETURN, "return"},
		{lexer.IDENTIFIER, "x"},
		{lexer.PLUS, "+"},
		{lexer.IDENTIFIER, "y"},
		{lexer.SEMICOLON, ";"},
		{lexer.RBRACE, "}"},
		{lexer.IDENTIFIER, "sum"},
		{lexer.INCREMENT, "++"},
		{lexer.SEMICOLON, ";"},
		{lexer.IF, "if"},
		{lexer.LPAREN, "("},
		{lexer.IDENTIFIER, "sum"},
		{lexer.GREATEREQ, ">="},
		{lexer.INTEGER, "13"},
		{lexer.RPAREN, ")"},
		{lexer.LBRACE, "{"},
		{lexer.RETURN, "return"},
		{lexer.IDENTIFIER, "sum"},
		{lexer.SEMICOLON, ";"},
		{lexer.RBRACE, "}"},
		{lexer.RETURN, "return"},
		{lexer.INTEGER, "0"},
		{lexer.SEMICOLON, ";"},
	}

	lexer_ := lexer.New(input)
	for iter, testToken := range tests {
		token := lexer_.NextToken()

		if token.Type != testToken.expectedType {
			t.Fatalf("Wrong token type at test: %d. \nexpected: %q, got: %q",
				iter, testToken.expectedType, token.Type)
		}

		if token.Value != testToken.expectedValue {
			t.Fatalf("Wrong literal vaulue at test: %d. \nexpected: %q, got: %q",
				iter, testToken.expectedValue, token.Value)
		}
	}
}
