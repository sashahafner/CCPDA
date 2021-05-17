# Compiles book

knitr::knit('CCPDA_book.Rnw'); system('texi2pdf CCPDA_book.tex')
knitr::knit('solutions.Rnw'); system('texi2pdf solutions.tex')
