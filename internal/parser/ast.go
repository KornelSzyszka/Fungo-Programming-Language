package parser

import (
	"Fungo/internal/lexer"
	"bytes"
)

type Node interface {
	TokenLiteral() string
	String() string
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

func (program *Program) String() string {
	var output bytes.Buffer

	for _, statement := range program.Statements {
		output.WriteString(statement.String())
	}

	return output.String()
}

type Identifier struct {
	Token lexer.Token
	Value string
}

func (ident *Identifier) expressionNode() {}
func (ident *Identifier) TokenLiteral() string {
	return ident.Token.Value
}
func (ident *Identifier) String() string {
	return ident.Value
}

type VarType struct {
	Token lexer.Token
	Value string
}

func (varType *VarType) expressionNode() {}
func (varType *VarType) TokenLiteral() string {
	return varType.Token.Value
}
func (varType *VarType) String() string {
	return varType.Value
}

type VarStatement struct {
	Token   lexer.Token
	Name    *Identifier
	VarType *VarType
	Value   Expression
}

func (statement *VarStatement) StatementNode() {}
func (statement *VarStatement) TokenLiteral() string {
	return statement.Token.Value
}
func (statement *VarStatement) String() string {
	var output bytes.Buffer

	output.WriteString(statement.TokenLiteral() + " ")
	output.WriteString(statement.Name.String() + ": ")

	if statement.VarType != nil {
		output.WriteString(statement.VarType.String())
	}

	output.WriteString(" = ")

	if statement.Value != nil {
		output.WriteString(statement.Value.String())
	}

	output.WriteString(";")

	return output.String()
}

type ReturnStatement struct {
	Token       lexer.Token
	ReturnValue Expression
}

func (statement *ReturnStatement) StatementNode() {}
func (statement *ReturnStatement) TokenLiteral() string {
	return statement.Token.Value
}
func (statement *ReturnStatement) String() string {
	var output bytes.Buffer

	output.WriteString(statement.TokenLiteral() + " ")

	if statement.ReturnValue != nil {
	}
	output.WriteString(statement.ReturnValue.String())

	output.WriteString(";")

	return output.String()
}

type ExpressionStatement struct {
	Token      lexer.Token
	Expression Expression
}

func (statement *ExpressionStatement) StatementNode() {}
func (statement *ExpressionStatement) TokenLiteral() string {
	return statement.Token.Value
}
func (statement *ExpressionStatement) String() string {
	if statement.Expression != nil {
		return statement.Expression.String()
	}
	return ""
}

type IntegerLiteral struct {
	Token lexer.Token
	Value int64
}

func (statement *IntegerLiteral) expressionNode() {}
func (statement *IntegerLiteral) TokenLiteral() string {
	return statement.Token.Value
}
func (statement *IntegerLiteral) String() string {
	return statement.Token.Value
}

type PrefixExpression struct {
	Token    lexer.Token
	Operator string
	Right    Expression
}

func (expression *PrefixExpression) expressionNode()      {}
func (expression *PrefixExpression) TokenLiteral() string { return expression.Token.Value }
func (expression *PrefixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(expression.Operator)
	out.WriteString(expression.Right.String())
	out.WriteString(")")
	return out.String()
}

type InfixExpression struct {
	Token    lexer.Token
	Left     Expression
	Operator string
	Right    Expression
}

func (expression *InfixExpression) expressionNode()      {}
func (expression *InfixExpression) TokenLiteral() string { return expression.Token.Value }
func (expression *InfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(expression.Left.String())
	out.WriteString(expression.Operator)
	out.WriteString(expression.Right.String())
	out.WriteString(")")
	return out.String()
}
