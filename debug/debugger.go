package debug

import (
	"Fungo/internal/lexer"
	"bufio"
	"fmt"
	"io"
)

const PROMPT = ">> "

func Run(input io.Reader, output io.Writer) {
	scanner := bufio.NewScanner(input)

	for {
		fmt.Printf(PROMPT)
		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		lexer_ := lexer.New(line)

		for token := lexer_.NextToken(); token.Type != lexer.EOF; token = lexer_.NextToken() {
			fmt.Printf("Type: %q , Value: %q\n", token.Type, token.Value)
		}
	}
}
