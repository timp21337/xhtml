
#lhs2TeX -o xhtml.tex xhtml.lhs



latex xhtml.tex
latex xhtml.tex

tex4ht  xhtml.tex


htlatex  xhtml.tex
pdflatex xhtml.tex



ghc -fhpc -o xhtml main.lhs xhtml.lhs 
