package main

import (
	"fmt"
	"math/rand"
)

func shuffle(nums []int) {
	// Функция `Seed()` инициализирует генератор случайных чисел
	// здесь мы используем константу `42`, чтобы программу
	// можно было проверить тестами.
	// В реальных задачах не используйте константы!
	// Используйте, например, время в наносекундах:
	// rand.Seed(time.Now().UnixNano())
	rand.Seed(42)
	// перетасуйте `nums` с помощью `rand.Shuffle()`
	rand.Shuffle(len(nums), func(i, j int) {
		buf := nums[i]
		nums[i] = nums[j]
		nums[j] = buf
	})
}

// ┌─────────────────────────────────┐
// │ не меняйте код ниже этой строки │
// └─────────────────────────────────┘

func main() {
	nums := readInput()
	shuffle(nums)
	fmt.Println(nums)
}
