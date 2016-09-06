all: assets pdf

assets:
	latexmk -pdf src/assets/binary-message-format -synctex=1 -outdir=bin/assets
	python src/scripts/generate_luci_documentation.py \
	  src/scripts/assets/documentation.html \
	  src/appendix/builtinservices.tex

pdf:
	latexmk -pdf src/specification -synctex=1 -outdir=bin

clean:
	rm -r -f bin/*.aux bin/*.log bin/*.bbl\
		bin/*.blg bin/*.toc bin/*.lof bin/*.lot\
		bin/*.out bin/*.fls bin/*.synctex.gz bin/*.fdb_latexmk

	rm -r -f bin/*/*.aux bin/*/*.log bin/*/*.bbl\
		bin/*.blg bin/*/*.toc bin/*/*.lof bin/*/*.lot\
		bin/*/*.out bin/*/*.fls bin/*/*.synctex.gz bin/*/*.fdb_latexmk
