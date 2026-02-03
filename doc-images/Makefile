TEXS := $(wildcard *.tex)
PDFS := $(TEXS:.tex=.pdf)
PNGS := $(TEXS:.tex=.png)

LATEXMK = latexmk -pdf -interaction=nonstopmode
MAGICK  = magick -density 300

all: $(PNGS)

%.pdf: %.tex
	$(LATEXMK) $<

%.png: %.pdf
	$(MAGICK) $< $@

clean:
	latexmk -C
	rm -f *.png
