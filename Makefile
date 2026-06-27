.PHONY: all env clean chr
.PRECIOUS: images/tiles.bmp images/tiles2.bmp images/tiles3.bmp images/tiles4.bmp

CHRUTIL = ../go-nes/bin/chrutil

NAME = tetris
NESCFG = nes_mmc5.cfg
CAFLAGS = -g -t nes
LDFLAGS = -C $(NESCFG) --dbgfile bin/$(NAME).dbg -m bin/$(NAME).map

SOURCES = \
	audio/sfx.s \
	audio/songs.s \
	basic-menu.asm \
	debug-field.i \
	famistudio_ca65.s \
	game-shared.asm \
	game-solo.asm \
	game-vs.asm \
	main.asm \
	menu-main.asm \
	menu-mode.asm \
	menu-mode.i \
	menu-practice.asm \
	menu-screen.i \
	new-high-score-a.i \
	new-high-score-b.i \
	options.asm \
	piece-rng.inc \
	playfield-rle.i \
	playfield-vs.i \
	practice-boards.i \
	scores-screen.i \
	scores.asm \
	utils.asm

CHR = tiles.chr tiles2.chr tiles3.chr tiles4.chr

all: env chr bin/$(NAME).nes
env: bin/
utils: bin/convert-map bin/practice-boards

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

playfield-vs.i: screens/vs-playfield.tmx utils
	bin/convert-map $< $@ --rle

playfield-rle.i: screens/playfield.tmx utils
	bin/convert-map $< $@ --rle

scores-screen.i: screens/scores.tmx utils
	bin/convert-map $< $@ --rle

new-high-score-a.i: screens/new-high-score.tmx utils
	bin/convert-map $< $@ --rle

new-high-score-b.i: screens/new-high-score.tmx utils
	bin/convert-map $< $@ --rle --layer shifted

debug-field.i: screens/debug-field.tmx utils
	bin/convert-map $< $@

menu-screen.i: screens/main-menu.tmx utils
	bin/convert-map $< $@ --rle

menu-mode.i: screens/menu-mode.tmx utils
	bin/convert-map $< $@ --rle

practice-boards.i: screens/practice-boards.tmx utils
	bin/practice-boards $< $@

audio/sfx.s: audio/sfx.fms
	famistudio $< famistudio-asm-sfx-export $@ -famistudio-asm-format:ca65 -famistudio-asm-sfx-generate-list:audio/sfx-names.s

bin/convert-map: convert-map/*.go
	cd convert-map && go build -o ../$@

bin/practice-boards: practice-boards/*.go
	cd practice-boards && go build -o ../$@
