package parser

import (
	"Fungo/internal/lexer"
)

type Node interface {
	TokenLiteral() string
}

type Statement interface {
	Node
	StatementNode()
}

type Expression interface {
	Node
	expressionNode()
}

type Program struct {
	Statements []Statement
}

func (program *Program) TokenLiteral() string {
	if len(program.Statements) > 0 {
		return program.Statements[0].TokenLiteral()
	}
	return ""
}

type Identifier struct {
	Token lexer.Token
	Value string
}

func (ident *Identifier) ExpressionNode() {}
func (ident *Identifier) TokenLiteral() string {
	return ident.Token.Value
}

type VarStatement struct {
	Token lexer.Token
	Name  *Identifier
	Value Expression
}

func (statement *VarStatement) StatementNode() {}
func (statement *VarStatement) TokenLiteral() string {
	return statement.Token.Value
}
