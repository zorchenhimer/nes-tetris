package main

import (
	"os"
	"fmt"
	"strings"

	"github.com/alexflint/go-arg"
	"github.com/zorchenhimer/go-tiled"
)

type options struct {
	Input string `arg:"positional,required"`
	Output string `arg:"positional,required"`
}

func main() {
	opts := &options{}
	arg.MustParse(opts)

	mapData, err := tiled.LoadMap(opts.Input)
	if err != nil {
		fmt.Println(err)
		return
	}

	output, err := os.Create(opts.Output)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer output.Close()

	fmt.Fprint(output, ".byte ")

	vals := []string{}
	for _, d := range mapData.Layers[0].Data {
		if d > 0 {
			d -= 1
		}
		vals = append(vals, fmt.Sprintf("$%02X", d))
	}
	fmt.Fprintln(output, strings.Join(vals, ", "))
}
