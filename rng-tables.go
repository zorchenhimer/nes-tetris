package main

import (
	"fmt"
	//"os"
	"math/rand"
	"time"
	"strings"
)

type Bag struct {
	items []byte
}

func NewBag(vals []byte) *Bag {
	return &Bag{ items: vals }
}

func (b *Bag) Len() int {
	return len(b.items)
}

func (b *Bag) Swap(i, j int) {
	b.items[i], b.items[j] = b.items[j], b.items[i]
}

func (b *Bag) AddTo(list []byte) []byte {
	return append(list, b.items...)
}

func main() {
	table := []byte{}
	rng := rand.New(rand.NewSource(time.Now().Unix()))

	for x := 0; x < 36; x++ {
		b := NewBag([]byte{0, 1, 2, 3, 4, 5, 6})
		rng.Shuffle(b.Len(), b.Swap)
		table = b.AddTo(table)
	}

	bagtop := []byte{2, 2, 5, 6}
	rng.Shuffle(len(bagtop), func(i, j int) { table[i], table[j] = table[j], table[i] })
	table = append(table, bagtop...)

	str := []string{}
	for _, n := range table {
		str = append(str, fmt.Sprintf("%d", n))
	}
	fmt.Println(".byte", strings.Join(str, ", "))
}
