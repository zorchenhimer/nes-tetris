package main

import (
	"os"
	"fmt"
	"strings"
	"strconv"
	"io"
	"hash/crc32"

	"github.com/alexflint/go-arg"
	"github.com/zorchenhimer/go-tiled"
)

type Arguments struct {
	Input  string `arg:"positional,required"`
	Output string `arg:"positional,required"`
}

func main() {
	args := &Arguments{}
	arg.MustParse(args)

	if err := run(args); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

type PracticeBoard struct {
	Name string
	Data []byte
}

func (pb PracticeBoard) NameLabel() string {
	return fmt.Sprintf("PB_name_%08X", crc32.ChecksumIEEE(pb.Data))
}

func (pb PracticeBoard) DataLabel() string {
	return fmt.Sprintf("PB_data_%08X", crc32.ChecksumIEEE(pb.Data))
}

func (pb PracticeBoard) WriteTableEntry(w io.Writer) error {
	_, err := fmt.Fprintf(w, "    ; %s\n    .word %s\n    .word %s\n\n", pb.Name, pb.NameLabel(), pb.DataLabel())
	return err
}

func (pb PracticeBoard) WriteData_old(w io.Writer) error {
	_, err := fmt.Fprintf(w, "%s:\n    .asciiz %q\n", pb.NameLabel(), pb.Name)
	if err != nil {
		return err
	}

	data := []string{}
	for _, b := range pb.Data {
		data = append(data, strconv.Itoa(int(b)))
	}

	_, err = fmt.Fprintf(w, "%s:\n    .byte %s\n", pb.DataLabel(), strings.Join(data, ", "))
	return err
}

func (pb PracticeBoard) WriteData(w io.Writer) error {
	_, err := fmt.Fprintf(w, "%s:\n    .asciiz %q\n", pb.NameLabel(), pb.Name)
	if err != nil {
		return err
	}

	data := []string{}
	var outbyte byte
	j := 0
	for _, b := range pb.Data {
		v := byte(0)
		if b != 0 {
			v = 1
		}
		outbyte = (outbyte << 1) | v
		fmt.Print(v, " ")
		//if j % 10 == 0 {
		//	fmt.Println("")
		//}

		//if j % 8 == 0 && j != 1 {
		if j == 8 {
			j = 0
			data = append(data, fmt.Sprintf("%%%08b", outbyte))
			fmt.Println("")
		}
		j++
	}
	fmt.Println("")
	data = append(data, fmt.Sprintf("%%%08b", outbyte))

	lines := []string{}
	line := []string{}
	j = 0
	fmt.Println("len(data):", len(data))
	for _, d := range data {
		//if j % 5 == 0 && j != 1 {
		if j == 5 {
			j = 0
			lines = append(lines, "    .byte "+strings.Join(line, ", "))
			//fmt.Println(line)
			line = []string{}
		}
		j++

		line = append(line, d)
	}
	lines = append(lines, "    .byte "+strings.Join(line, ", "))

	//_, err = fmt.Fprintf(w, "%s:\n    .byte %s\n", pb.DataLabel(), strings.Join(data, ", "))
	_, err = fmt.Fprintf(w, "%s:\n%s\n", pb.DataLabel(), strings.Join(lines, "\n"))
	return err
}

func run(args *Arguments) error {
	allBoards, err := tiled.LoadMap(args.Input)
	if err != nil {
		return err
	}

	boards := []PracticeBoard{}
	for _, layer := range allBoards.Layers {
		name := layer.Properties.GetStringProperty("Name", "")
		if name == "" {
			fmt.Printf("Layer %q missing 'Name' property. Skipping.", layer.Name)
			continue
		}

		data := []byte{}
		for _, d := range layer.Data {
			val := d
			if val != 0 {
				val -= 1
			}
			data = append(data, byte(val&255))
		}

		boards = append(boards, PracticeBoard{Name: name, Data: data})
	}

	output, err := os.Create(args.Output)
	if err != nil {
		return err
	}
	defer output.Close()

	fmt.Fprintln(output, "PracticeBoards:")

	for _, board := range boards {
		err = board.WriteTableEntry(output)
		if err != nil {
			return err
		}
	}

	fmt.Fprintln(output, "    .word $0000\n\n")

	for _, board := range boards {
		err = board.WriteData(output)
		if err != nil {
			return err
		}
		fmt.Fprintln(output, "")
	}

	return nil
}
