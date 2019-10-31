#!/bin/sh

set -ev

Rscript -e "usethis::git_vaccinate()"
Rscript -e "usethis::use_git_ignore(ignores=c('.gitignore', '.directory', 'bookdown-demo*', 'docs-local', '_bookdown_files/'))"
Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
#Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::pdf_book')"
#Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::epub_book')"

