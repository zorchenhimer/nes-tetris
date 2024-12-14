.PHONY: all env clean chr

CHRUTIL = ../go-nes/bin/chrutil

NAME = tetris
NESCFG = nes_nrom.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = \
	main.asm \
	menu.asm \
	scores.asm \
	game.asm \
	playfield.i

CHR = tiles.chr

all: env chr bin/$(NAME).nes
env: bin/

clean:
	-rm bin/* *.chr *.i

cleanall: clean
	-rm images/*.bmp *.inc
	-$(MAKE) -C go-nes/ clean

bin/:
	-mkdir bin

bin/$(NAME).nes: bin/main.o
	ld65 $(LDFLAGS) -o $@ $^

bin/main.o: $(SOURCES) $(CHR)
	ca65 $(CAFLAGS) -o $@ main.asm

%.chr: images/%.bmp
	$(CHRUTIL) $< -o $@

images/%.bmp: images/%.aseprite
	aseprite -b $< --save-as $@

playfield.i: screens/playfield.tmx
	go run convert-map.go $< $@
