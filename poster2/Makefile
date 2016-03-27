ALL: poster2.pdf clean

%.pdf: %.tex Makefile
	pdflatex $< && bibtex $(<:.tex=.aux) && pdflatex $< && pdflatex $<

clean:
	  rm -f *.aux *.bbl *.blg *.log  *.nav *.out *.snm *.toc

sclean: clean
	rm -f poster2.pdf
