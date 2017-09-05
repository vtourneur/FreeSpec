NAME        := $(shell basename `pwd`)
PROJECT     := _CoqProject
FLIST       := Files
SRC         := $(shell cat $(FLIST))
SUBMAKE     := Makefile.proj
COQDOCFLAGS :=                                      \
  --toc --toc-depth 2 --html --interpolate         \
  --index indexpage --no-lib-name --parse-comments \
  --with-header docs/assets/header.html            \
  --with-footer docs/assets/footer.html

export COQDOCFLAGS

.PHONY:all clean mrproper docs html tex hextraction

all: hextraction $(SUBMAKE) $(SRC)
	@(echo "[*] Compiling the project")
	@(make -f $(SUBMAKE))

$(SUBMAKE): .make
	@(echo "[*] Generating $(SUBMAKE)")
	@(coq_makefile -f .make -o $@)

.make: $(FLIST) $(PROJECT)
	@(rm -f $@)
	@(cat $(PROJECT) >> .make)
	@(cat $(FLIST) >> .make)

clean: $(SUBMAKE)
	make -f $(SUBMAKE) clean
	make -C libs/haskell-extraction clean

mrproper: clean
	rm .make
	rm -rf docs/html
	rm -f docs/$(NAME).pdf
	rm -f $(SUBMAKE)

docs: html tex

html:
	rm -rf docs/html
	make -f $(SUBMAKE) html
	mv html docs/
	cp docs/assets/coqdoc.css docs/html
	cp docs/assets/coqdocjs.css docs/html
	cp docs/assets/coqdocjs.js docs/html
	cp docs/assets/config.js docs/html

tex:
	make -f $(SUBMAKE) all-gal.pdf
	mv all-gal.pdf docs/$(NAME).pdf

hextraction:
	make -C libs/haskell-extraction
