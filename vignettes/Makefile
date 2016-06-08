# I use this Makefile to speed up local development of the vignette.

html:
	R -e "knitr::knit2html('piwikr.Rmd')"

blog:
	R -e "knitr::knit('piwikr.Rmd')"; ~/Dropbox/bin/blog.sh piwikr figure
