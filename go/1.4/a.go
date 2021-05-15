package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func filter(predicate func(int) bool, iterable []int) []int {
	var res []int
	for _, el := range iterable {
		if predicate(el) {
			res = append(res, el)
		}
	}
	return res
}

func main() {
	src := readInput()
	res := filter(func(el int) bool {
		return el%2 == 0
	}, src)
	fmt.Println(res)
}

// ┌─────────────────────────────────┐
// │ не меняйте код ниже этой строки │
// └─────────────────────────────────┘

// readInput считывает целые числа из `os.Stdin`
// и возвращает в виде среза
// разделителем чисел считается пробел
func readInput() []int {
	var nums []int
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		num, _ := strconv.Atoi(scanner.Text())
		nums = append(nums, num)
	}
	return nums
}
