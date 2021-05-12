package main

import (
	"fmt"
)

func main() {
	var text string
	var width int
	fmt.Scanf("%s %d", &text, &width)

	// Возьмите первые `width` байт строки `text`,
	// допишите в конце `...` и сохраните результат
	// в переменную `res`
	// ...
	res := ""

	if len(text) <= 6 {
		fmt.Println(text)
		return
	}

	for i := 0; i < width; i++ {
		res = res + string(text[i])
	}

	fmt.Println(res + "...")
}
