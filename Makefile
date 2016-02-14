# I use this Makefile to speed up local development of the code and
# vignette.

install:
	R CMD INSTALL .

vignettes/piwikr.html: vignettes/piwikr.Rmd
	cd vignettes; R -e "knitr::knit2html('piwikr.Rmd')"

blog: vignettes/piwikr.Rmd
	cd vignettes; R -e "knitr::knit('piwikr.Rmd')"; ~/Dropbox/blog.sh piwikr figure
