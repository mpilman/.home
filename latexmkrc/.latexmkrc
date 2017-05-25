$pdflatex = 'lualatex -synctex=1 %O %S';
$pdf_mode = 1;
$bibtex_use = 2;
# User biber instead of bibtex
$biber = 'biber --debug %O %S';
# Extra file extensions to clean when latexmk -c or latexmk -C is used
$clean_ext = '%R.run.xml %R.synctex.gz';

