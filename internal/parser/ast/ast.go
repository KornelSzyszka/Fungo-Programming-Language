package ast

import (
	"Fungo/internal/lexer/token"
	"bytes"
)

// Node represents the basic interface for all nodes in the AST.
// All nodes must implement TokenLiteral and String methods.
type Node interface {
	TokenLiteral() string
	String() string
}

// Statement represents a node that is a statement.
// Statements must implement the Node interface and StatementNode.
type Statement interface {
	Node
	StatementNode()
}

// Expression represents a node that is an expression.
// Expressions must implement the Node interface and expressionNode.
type Expression interface {
	Node
	expressionNode()
}

// Program represents the root node of the AST.
// It consists of a slice of statements.
type Program struct {
	Statements []Statement
}

// TokenLiteral returns the literal value of the first statement's token, if any.
// If there are no statements, it returns an empty string.
func (program *Program) TokenLiteral() string {
	if len(program.Statements) > 0 {
		return program.Statements[0].TokenLiteral()
	}
	return ""
}

// String returns a string representation of the program by concatenating
// the string representations of all statements.
func (program *Program) String() string {
	var output bytes.Buffer

	for _, statement := range program.Statements {
		output.WriteString(statement.String())
	}

	return output.String()
}

// Identifier represents an identifier node, such as a variable name.
// It holds a token and its string value.
type Identifier struct {
	Token token.Token
	Value string
}

func (ident *Identifier) expressionNode() {}

// TokenLiteral returns the literal value of the identifier's token.
func (ident *Identifier) TokenLiteral() string {
	return ident.Token.Value
}

// String returns the identifier's value.
func (ident *Identifier) String() string {
	return ident.Value
}

// VarType represents a variable type node, holding a token and its type as a string.
type VarType struct {
	Token token.Token
	Value string
}

func (varType *VarType) expressionNode() {}

// TokenLiteral returns the literal value of the VarType's token.
func (varType *VarType) TokenLiteral() string {
	return varType.Token.Value
}

// String returns the variable type's value as a string.
func (varType *VarType) String() string {
	return varType.Value
}

// VarStatement represents a variable declaration statement, consisting of
// a token, a name (Identifier), an optional type, and an initial value.
type VarStatement struct {
	Token   token.Token
	Name    *Identifier
	VarType *VarType
	Value   Expression
}

func (statement *VarStatement) StatementNode() {}

// TokenLiteral returns the literal value of the variable statement's token.
func (statement *VarStatement) TokenLiteral() string {
	return statement.Token.Value
}

// String returns the variable declaration as a string in the form:
// `var <name>: <type> = <value>;`
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

// ReturnStatement represents a return statement in the AST.
// It holds a token and an optional return value expression.
type ReturnStatement struct {
	Token       token.Token
	ReturnValue Expression
}

func (statement *ReturnStatement) StatementNode() {}

// TokenLiteral returns the literal value of the return statement's token.
func (statement *ReturnStatement) TokenLiteral() string { return statement.Token.Value }

// String returns the return statement as a string in the form:
// `return <value>;`
func (statement *ReturnStatement) String() string {
	var output bytes.Buffer

	output.WriteString(statement.TokenLiteral() + " ")

	if statement.ReturnValue != nil {
		output.WriteString(statement.ReturnValue.String())
	}

	output.WriteString(";")

	return output.String()
}

// ExpressionStatement represents a statement that consists solely of an expression.
type ExpressionStatement struct {
	Token      token.Token
	Expression Expression
}

func (statement *ExpressionStatement) StatementNode() {}

// TokenLiteral returns the literal value of the expression statement's token.
func (statement *ExpressionStatement) TokenLiteral() string { return statement.Token.Value }

// String returns the expression as a string.
func (statement *ExpressionStatement) String() string {
	if statement.Expression != nil {
		return statement.Expression.String()
	}
	return ""
}

// BlockStatement represents a block of statements in the AST,
// consisting of a token and a slice of contained statements.
type BlockStatement struct {
	Token      token.Token // The token representing the opening brace '{'.
	Statements []Statement // A slice of statements within the block.
}

func (blockStatement *BlockStatement) StatementNode() {}

// TokenLiteral returns the literal value of the block statement's token.
func (blockStatement *BlockStatement) TokenLiteral() string {
	return blockStatement.Token.Value
}

// String returns the block statement as a string,
// concatenating the string representations of all statements.
func (blockStatement *BlockStatement) String() string {
	var output bytes.Buffer

	for _, statement := range blockStatement.Statements {
		output.WriteString(statement.String())
	}
	return output.String()
}

// IntegerLiteral represents an integer literal in the AST.
type IntegerLiteral struct {
	Token token.Token
	Value int64
}

func (statement *IntegerLiteral) expressionNode() {}

// TokenLiteral returns the literal value of the integer literal's token.
func (statement *IntegerLiteral) TokenLiteral() string {
	return statement.Token.Value
}

// String returns the integer literal's value as a string.
func (statement *IntegerLiteral) String() string {
	return statement.Token.Value
}

// Boolean represents a boolean literal in the AST.
type Boolean struct {
	Token token.Token
	Value bool
}

func (boolean *Boolean) expressionNode() {}

// TokenLiteral returns the literal value of the boolean's token.
func (boolean *Boolean) TokenLiteral() string { return boolean.Token.Value }

// String returns the boolean value as a string.
func (boolean *Boolean) String() string { return boolean.Token.Value }

// PrefixExpression represents a prefix expression in the AST.
// It consists of an operator and a right-hand expression.
type PrefixExpression struct {
	Token    token.Token
	Operator string
	Right    Expression
}

func (expression *PrefixExpression) expressionNode() {}

// TokenLiteral returns the literal value of the prefix expression's token.
func (expression *PrefixExpression) TokenLiteral() string { return expression.Token.Value }

// String returns the prefix expression as a string in the form:
// `(<operator><right expression>)`
func (expression *PrefixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(expression.Operator)
	out.WriteString(expression.Right.String())
	out.WriteString(")")
	return out.String()
}

// InfixExpression represents an infix expression in the AST.
// It consists of a left-hand expression, an operator, and a right-hand expression.
type InfixExpression struct {
	Token    token.Token
	Left     Expression
	Operator string
	Right    Expression
}

func (expression *InfixExpression) expressionNode() {}

// TokenLiteral returns the literal value of the infix expression's token.
func (expression *InfixExpression) TokenLiteral() string { return expression.Token.Value }

// String returns the infix expression as a string in the form:
// `(<left expression> <operator> <right expression>)`
func (expression *InfixExpression) String() string {
	var out bytes.Buffer
	out.WriteString("(")
	out.WriteString(expression.Left.String())
	out.WriteString(expression.Operator)
	out.WriteString(expression.Right.String())
	out.WriteString(")")
	return out.String()
}

// IFExpression represents an if-else expression in the AST.
type IFExpression struct {
	Token       token.Token
	Condition   Expression
	Consequence *BlockStatement
	Alternative *BlockStatement
}

func (expression *IFExpression) expressionNode() {}

// TokenLiteral returns the literal value of the if-else expression's token.
func (expression *IFExpression) TokenLiteral() string { return expression.Token.Value }

// String returns the string representation of the IFExpression node.
// It constructs a formatted string representing an `if` expression in the form:
// 'if <condition> <consequence> [else <alternative>]'
func (expression *IFExpression) String() string {
	var output bytes.Buffer

	output.WriteString("if")
	output.WriteString(expression.Condition.String())
	output.WriteString(" ")
	output.WriteString(expression.Consequence.String())

	if expression.Alternative != nil {
		output.WriteString("else ")
		output.WriteString(expression.Alternative.String())
	}

	return output.String()
}
