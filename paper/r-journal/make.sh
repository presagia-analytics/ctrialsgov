#!/bin/zsh

R -e "rmarkdown::render('arnold-wei-jiang-kane.Rmd')"
rm RJwrapper.log
rm arnold-wei-jiang-kane.R
rm arnold-wei-jiang-kane.log
rm arnold-wei-jiang-kane.tex
rm RJwrapper.tex
