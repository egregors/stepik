package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()
	// выведите день недели в формате
	// Today is Monday
	// используйте now.Weekday(), чтобы определить день недели
	// используйте fmt.Println() для вывода
	// ...
	fmt.Println("Today is", now.Weekday())
}
