# piwikr

[![Build Status](https://travis-ci.org/amarder/piwikr.svg?branch=master)](https://travis-ci.org/amarder/piwikr)
[![Coverage Status](https://coveralls.io/repos/github/amarder/piwikr/badge.svg?branch=master)](https://coveralls.io/github/amarder/piwikr?branch=master)

If you like [Piwik](http://piwik.org/) and [R](https://www.r-project.org/) then `piwikr` is for you. [This post](http://amarder.github.io/piwikr/) describes some of the custom analytics possible using `piwikr`.

## Installation

To install `piwikr` in `R` use the following commands:

``` r
install.packages('devtools')
devtools::install_github('amarder/piwikr')
```

## Notes

[This page](https://developer.piwik.org/guides/persistence-and-the-mysql-backend) was useful in understanding the data stored by Piwik in MySQL.

[This Stack Overflow answer](http://stackoverflow.com/a/12429344/3756632) describes my current approach to suppressing notes from `R CMD check` that are created by my use of non-standard evaluation.
