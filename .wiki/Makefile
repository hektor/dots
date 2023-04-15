NOTES := $(wildcard *.md)
AUXFILES := ./img/

target=wiki
exports=$(NOTES:.md=.html)

WIKI_DIR=/home/h/.wiki

%.html : %.md
	pandoc \
    -f markdown+latex_macros-yaml_metadata_block+abbreviations+emoji+pipe_tables \
    -t html5 \
    -c pandoc.css \
    --citeproc \
    --metadata-file=meta.yaml \
    --template=$(WIKI_DIR)/template \
    --bibliography $(WIKI_DIR)/references.bib \
    --resource-path=$(WIKI_DIR)/img/ \
    --lua-filter=$(WIKI_DIR)/filters/html-links.lua \
    --lua-filter=$(WIKI_DIR)/filters/tikz.lua \
    --lua-filter=$(WIKI_DIR)/filters/zotero.lua \
    --lua-filter=$(WIKI_DIR)/lua-filters/diagram-generator/diagram-generator.lua \
    --lua-filter=$(WIKI_DIR)/lua-filters/lilypond/lilypond.lua \
    --toc \
    --markdown-headings=atx \
    --extract-media=img \
    --strip-comments \
    --file-scope \
    -s -o "$@" "$<"

.PHONY: all clean

all: $(exports)
clean: rm $(exports)
