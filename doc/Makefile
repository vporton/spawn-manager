DESTDIR   ?= html
STYLEPATH ?= .
PREPARE   := $(shell mkdir -p $(DESTDIR))

ASCIIDOC_OPTS = \
	--backend=xhtml11 \
	--conf-file=page.conf \
	-a stylesdir=$(STYLEPATH) \
	-a linkcss \
	-a disable-javascript \
	-o -

all: $(DESTDIR)/asciidoc.css $(DESTDIR)/index.html

$(DESTDIR)/asciidoc.css: asciidoc.css
	cp $< $@

$(DESTDIR)/index.html: ../README page.conf
	asciidoc $(ASCIIDOC_OPTS) $< > $@

clean:
	@rm -f $(DESTDIR)/*.html
	@rm -f $(DESTDIR)/*.css

distclean:
	@rm -rf $(DESTDIR)

PHONY: clean distclean
