package main

import (
	"bufio"
	"fmt"
	"os"
)

func READ(str string) string  { return str }
func EVAL(str string) string  { return str }
func PRINT(str string) string { return str }
func rep(str string) string   { return str }

func main() {
	//	var str string
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Print("user> ")
		text, err := reader.ReadString('\n')
		if err != nil {
			fmt.Println("Error scanning: ", err)
			//			break
		}
		fmt.Print(rep(text))
	}
}
