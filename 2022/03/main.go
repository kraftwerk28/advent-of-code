package main

import (
	"flag"
	"fmt"
)

func main() {
	amountFlag := flag.Int("amount", 42, "Specify amount")
	doStuffFlag := flag.Bool("dostuff", true, "Specify if do stuff")
	flag.Parse()
	fmt.Printf("amount = %v; do stuff = %v\n", *amountFlag, *doStuffFlag)
	fmt.Println("args = ", flag.Args())
}
