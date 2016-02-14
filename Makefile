# I use this Makefile to speed up local development of the code and
# vignette.

install:
	R CMD INSTALL .

vignettes/piwikr.html: vignettes/piwikr.Rmd
	cd vignettes; R -e "rmarkdown::render('piwikr.Rmd')"
