package main

import (
	"fmt"
	//"os"
	"math/rand"
	"time"
	"strings"
)

func main() {
	table := []byte{}
	for i := 0; i < 36; i++ {
		table = append(table, []byte{0, 1, 2, 3, 4, 5, 6}...)
	}

	table = append(table, []byte{2, 2, 5, 6}...)

	rng := rand.New(rand.NewSource(time.Now().Unix()))
	shuf := func(i, j int) { table[i], table[j] = table[j], table[i] }
	rng.Shuffle(len(table), shuf)

	str := []string{}
	for _, n := range table {
		str = append(str, fmt.Sprintf("%d", n))
	}
	fmt.Println(".byte", strings.Join(str, ", "))
}
