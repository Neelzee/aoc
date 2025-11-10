package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"unicode"
)

func main() {
	file, err := os.Open("./data")
	if err != nil {
		fmt.Println("Error: ", err)
		return
	}

	defer file.Close()

	scanner := bufio.NewScanner(file)

	sum_1 := 0
	sum_2 := 0

	for scanner.Scan() {
		line := scanner.Text()
		a_1, b_1 := parse_word([]rune(line))
		a_2, b_2 := get_numbers([]rune(line))
		fmt.Printf("%v, %v in: %v\n", a_2, b_2, line)
		sum_1 += a_1 * 10 + b_1
		sum_2 += a_2 * 10 + b_2
	}
	fmt.Printf("Star 1: %v\n", sum_1)
	fmt.Printf("Star 2: %v\n", sum_2)
}



func get_numbers(runes []rune) (int, int) {
	string_numbers := []string{"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"}
	a := -1
	b := -1

	for i, r := range runes {
		if unicode.IsDigit(r) && a == -1 {
			a = int(r) - 48
		} else if a == -1 {
			for j, n := range string_numbers {
				p := strings.Index(string(runes[0:i + 1]), n)

				if p != -1 {
					a = j + 1
					break
				}
			}
		}

		ind := len(runes) - 1 - i
		l := runes[ind]

		if unicode.IsDigit(l) && b == -1 {
			b = int(l) - 48
		} else if b == -1 {
			for j, n := range string_numbers {
				p := strings.Index(string(runes[ind:]), n)

				if p != -1 {
					b = j + 1
					break
				}
			}
		}
	}

	if b == -1 {
		return a, a
	}
	return a, b
}

func parse_word(line []rune) (int, int) {
	a := -1
	b := -1
	for i, r := range line {

		l := line[len(line) - i - 1]

		if a == -1 && unicode.IsDigit(r) {
			a = int(r) - 48
		}

		if b == -1 && unicode.IsDigit(l) {
			b = int(l) - 48
		}

		if a != -1 && b != -1 {
			return a, b
		}
	}

	return a, b
}



func Map[A any, B any](slice []A, fn func(A) B) []B {
    result := make([]B, len(slice))
    for i, v := range slice {
        result[i] = fn(v)
    }
    return result
}

func Max(slice []int) int {
    if len(slice) == 0 {
        return 0 // or panic, or return an error depending on your use case
    }
    max := slice[0]
    for _, v := range slice {
        if v > max {
            max = v
        }
    }
    return max
}
