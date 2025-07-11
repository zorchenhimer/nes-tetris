package main

import (
	"os"
	"fmt"
	"strings"
	"io"

	"github.com/alexflint/go-arg"
	"github.com/zorchenhimer/go-tiled"
)

type options struct {
	Input  string `arg:"positional,required"`
	Output string `arg:"positional,required"`
	RLE    bool   `arg:"--rle" help:"Use run length encoding"`
	Layer  string `arg:"--layer" help"Convert this named layer"`
}

func main() {
	args := &options{}
	arg.MustParse(args)

	mapData, err := tiled.LoadMap(args.Input)
	if err != nil {
		fmt.Println(err)
		return
	}

	output, err := os.Create(args.Output)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer output.Close()

	layerId := 0
	if args.Layer != "" {
		for i, l := range mapData.Layers {
			if l.Name == args.Layer {
				layerId = i
			}
		}
	}

	if args.RLE {
		encode(mapData.Layers[layerId].Data, output)
		return
	}

	fmt.Fprint(output, ".byte ")
	vals := []string{}
	for _, d := range mapData.Layers[layerId].Data {
		if d > 0 {
			d -= 1
		}
		vals = append(vals, fmt.Sprintf("$%02X", d))
	}
	fmt.Fprintln(output, strings.Join(vals, ", "))
}

// $8x = repeat byte
//       [count] [value]
// $0x = raw bytes
//       [len] [data...]
func encode(data []uint32, output io.Writer) {
	for i := 0; i < len(data); i++ {
		// Found repeat
		if data[i] == data[i+1] {
			var d []uint8
			val := data[i]
			for ; i < len(data); i++ {
				if val != data[i] || len(d) > 126 {
					i--
					break
				}
				if val == 0 {
					d = append(d, uint8(data[i]))
				} else {
					d = append(d, uint8(data[i]-1))
				}
			}
			l := len(d)
			if val&255 == 0 {
				fmt.Fprintf(output, ".byte %2d | $80, $%02X\n", l, 0)
			} else {
				fmt.Fprintf(output, ".byte %2d | $80, $%02X\n", l, (val-1)&255)
			}

		// not repeat
		} else {
			var d []uint8
			var x int
			for x = i; x < len(data); x++ {
				if data[x] == data[x+1] || len(d) > 126 { // this can probably go OOB
					break
				}
				d = append(d, uint8(data[x]-1))
			}

			l := uint8(len(d))

			str := []string{}
			for _, v := range d {
				str = append(str, fmt.Sprintf("$%02X", v&255))
			}
			fmt.Fprintf(output, ".byte %2d | $00, %s\n", l, strings.Join(str, ", "))

			i = x-1
		}
	}

	fmt.Fprintln(output, ".byte $00")
	return
}
