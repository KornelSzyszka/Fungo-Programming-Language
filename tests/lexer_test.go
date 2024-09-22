package tests

import (
	"Fungo/internal/lexer"
	"Fungo/internal/lexer/token"
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
		expectedType  token.Type
		expectedValue string
	}{
		{token.VARIABLE, "var"},
		{token.IDENTIFIER, "five"},
		{token.COLON, ":"},
		{token.VARTYPE, "int"},
		{token.ASSIGN, "="},
		{token.INTEGER, "5"},
		{token.SEMICOLON, ";"},
		{token.VARIABLE, "var"},
		{token.IDENTIFIER, "ten"},
		{token.COLON, ":"},
		{token.VARTYPE, "int"},
		{token.ASSIGN, "="},
		{token.INTEGER, "10"},
		{token.SEMICOLON, ";"},
		{token.VARIABLE, "var"},
		{token.IDENTIFIER, "sum"},
		{token.COLON, ":"},
		{token.VARTYPE, "int"},
		{token.ASSIGN, "="},
		{token.FUNCTION, "func"},
		{token.IDENTIFIER, "add"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "x"},
		{token.COLON, ":"},
		{token.VARTYPE, "int"},
		{token.COMMA, ","},
		{token.IDENTIFIER, "y"},
		{token.COLON, ":"},
		{token.VARTYPE, "int"},
		{token.RPAREN, ")"},
		{token.ARROW, "->"},
		{token.VARTYPE, "int"},
		{token.LBRACE, "{"},
		{token.RETURN, "return"},
		{token.IDENTIFIER, "x"},
		{token.PLUS, "+"},
		{token.IDENTIFIER, "y"},
		{token.SEMICOLON, ";"},
		{token.RBRACE, "}"},
		{token.IDENTIFIER, "sum"},
		{token.INCREMENT, "++"},
		{token.SEMICOLON, ";"},
		{token.IF, "if"},
		{token.LPAREN, "("},
		{token.IDENTIFIER, "sum"},
		{token.GREATEREQ, ">="},
		{token.INTEGER, "13"},
		{token.RPAREN, ")"},
		{token.LBRACE, "{"},
		{token.RETURN, "return"},
		{token.IDENTIFIER, "sum"},
		{token.SEMICOLON, ";"},
		{token.RBRACE, "}"},
		{token.RETURN, "return"},
		{token.INTEGER, "0"},
		{token.SEMICOLON, ";"},
	}

	lexer_ := lexer.New(input)
	for iter, testToken := range tests {
		token_ := lexer_.NextToken()

		if token_.Type != testToken.expectedType {
			t.Fatalf("Wrong token type at test: %d. \nexpected: %q, got: %q",
				iter, testToken.expectedType, token_.Type)
		}

		if token_.Value != testToken.expectedValue {
			t.Fatalf("Wrong literal value at test: %d. \nexpected: %q, got: %q",
				iter, testToken.expectedValue, token_.Value)
		}
	}
}
