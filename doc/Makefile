MAN = time-sheet.3
PDF = time-sheet.pdf
HTML = time-sheet.html

COMMONOPTIONS = --self-contained --number-sections --normalize --smart --toc

TEX_META = -V date='\today' --highlight-style=monochrome
HTML_META = -c manual.css

MAIN_TEX_META = -V author="Frank Terbeck"
MAIN_TEX_META += -V title="time-sheet(3) Reference Manual"
MAIN_HTML_META = $(MAIN_TEX_META) --highlight-style=monochrome
MAIN_MAN_META = -V title="time-sheet" -V section="3" -V date="Sep, 2015"

all: man html pdf
man: $(MAN)
pdf: $(PDF)
html: $(HTML)

time-sheet.3: time-sheet.mdwn
	pandoc -t man $(COMMONOPTIONS) $(MAIN_MAN_META) -o $@ time-sheet.mdwn
time-sheet.pdf: time-sheet.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(MAIN_TEX_META) -o $@ time-sheet.mdwn
time-sheet.html: time-sheet.mdwn
	pandoc -t html5 $(HTML_META) $(COMMONOPTIONS) $(MAIN_HTML_META) -o $@ time-sheet.mdwn

clean:
	rm -f $(HTML) $(PDF) $(MAN) *~

.PHONY: all man html pdf clean
