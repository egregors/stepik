package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

// Определите недостающие типы:

// element - интерфейс элемента последовательности
// (пустой, потому что элемент может быть любым)
type element interface{}

// weightFunc - функция, которая возвращает вес элемента
type weightFunc func(e element) int

// iterable - интерфейс последовательности, которую можно поэлементно перебирать
type iterable interface {
	next() bool
	val() element
}

// Тип ints - последовательность из целых чисел
// (реализует интерфейс iterable)
type ints struct {
	xs  []int
	idx int
}

func (i ints) val() element {
	return i.xs[i.idx]
}

func (i *ints) next() bool {
	if i.idx < len(i.xs)-1 {
		i.idx++
		return true
	}
	return false
}

// конструктор ints
func newInts(xs []int) *ints {
	return &ints{
		xs:  xs,
		idx: -1,
	}
}

// ┌─────────────────────────────────────────────┐
// │ не меняйте код ниже этой строки             │
// └─────────────────────────────────────────────┘

// main находит максимальное число из переданных на вход программы.
func main() {
	nums := readInput()
	it := newInts(nums)
	weight := func(el element) int {
		return el.(int)
	}
	m := max(it, weight)
	fmt.Println(m)
}

// max возвращает максимальный элемент в последовательности it.
// Для сравнения элементов используется вес, который возвращает
// функция weight.
func max(it iterable, weight weightFunc) element {
	var maxEl element
	for it.next() {
		curr := it.val()
		if maxEl == nil || weight(curr) > weight(maxEl) {
			maxEl = curr
		}
	}
	return maxEl
}

// readInput считывает последовательность целых чисел из os.Stdin.
func readInput() []int {
	var nums []int
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		num, err := strconv.Atoi(scanner.Text())
		if err != nil {
			log.Fatal(err)
		}
		nums = append(nums, num)
	}
	return nums
}
