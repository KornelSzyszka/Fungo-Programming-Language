package main

import (
	"Fungo/debug"
	"fmt"
	"os"
	"os/user"
)

func main() {
	user_, err := user.Current()
	if err != nil {
		panic(err)
	}
	fmt.Printf("Hello %s!\n# This is FunGo #\n", user_.Username)
	debug.Run(os.Stdin, os.Stdout)
}
