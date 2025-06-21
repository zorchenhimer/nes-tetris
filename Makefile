.PHONY: all env clean chr
.PRECIOUS: images/tiles.bmp images/tiles2.bmp images/tiles3.bmp images/tiles4.bmp

CHRUTIL = ../go-nes/bin/chrutil

NAME = tetris
NESCFG = nes_mmc5.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = \
	main.asm \
	menu.asm \
	mode-menu.asm \
	menu-practice.asm \
	scores.asm \
	game.asm \
	game.inc \
	vsmode.asm \
	utils.asm \
	options.asm \
	piece-rng.inc \
	playfield-rle.i \
	menu-screen.i \
	mode-menu.i \
	scores-screen.i \
	new-high-score-a.i \
	new-high-score-b.i \
	playfield-vs.i \
	practice-boards.i \
	debug-field.i \
	basic-menu.asm

CHR = tiles.chr tiles2.chr tiles3.chr tiles4.chr

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

playfield-vs.i: screens/vs-playfield.tmx convert-map.go
	go run convert-map.go $< $@ --rle

playfield-rle.i: screens/playfield.tmx convert-map.go
	go run convert-map.go $< $@ --rle

scores-screen.i: screens/scores.tmx convert-map.go
	go run convert-map.go $< $@ --rle

new-high-score-a.i: screens/new-high-score.tmx convert-map.go
	go run convert-map.go $< $@ --rle

new-high-score-b.i: screens/new-high-score.tmx convert-map.go
	go run convert-map.go $< $@ --rle --layer shifted

debug-field.i: screens/debug-field.tmx
	go run convert-map.go $< $@

menu-screen.i: screens/main-menu.tmx
	go run convert-map.go $< $@ --rle

mode-menu.i: screens/mode-menu.tmx
	go run convert-map.go $< $@ --rle

practice-boards.i: screens/practice-boards.tmx practice-boards.go
	go run practice-boards.go $< $@
