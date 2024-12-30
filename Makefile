.PHONY: all env clean chr
.PRECIOUS: images/tiles2.bmp

CHRUTIL = ../go-nes/bin/chrutil

NAME = tetris
NESCFG = nes_mmc5.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = \
	main.asm \
	menu.asm \
	scores.asm \
	game.asm \
	utils.asm \
	piece-rng.inc \
	playfield.i \
	debug-field.i

CHR = tiles.chr tiles2.chr tiles3.chr

all: env chr bin/$(NAME).nes
env: bin/

send: all
	./edlink-n8 bin/$(NAME).nes

clean:
	-rm bin/* *.chr *.i

cleanall: clean
	-rm images/*.bmp *.inc
	-$(MAKE) -C go-nes/ clean

piece-rng.inc: rng-tables.go
	go run $< > $@

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

debug-field.i: screens/debug-field.tmx
	go run convert-map.go $< $@

