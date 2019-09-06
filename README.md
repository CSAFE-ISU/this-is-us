Hi everybody,

in order to avoid the most egregious problems with the repository, we are going to need a couple of rules how to work with each other. We will add to these as we need more rules :)

1. run `usethis::git_vaccinate()` in your RStudio session - this will add files to your global `.gitignore` file. Those are files that should not be included in any of your github repositories (for security reasons).

2. A lot of our troubles with pulling and merging the repo resulted from conflicts in the `docs` folder. `docs` is used to render the websites for the repo. We don't all have to render and re-render the book (a lot of issues came from different line endings between operating systems). Solution: we will have a local rendering of the book, so you can have a look at it, but we will only update websites just before show-and-tell. 
Run the command
`usethis::use_git_ignore(ignores=c("docs-local", "_bookdown_files/"))`
to make sure that git ignores your local book build. 



This is a minimal example of a book based on R Markdown and **bookdown** (https://github.com/rstudio/bookdown). Please see the page "[Get Started](https://bookdown.org/yihui/bookdown/get-started.html)" at https://bookdown.org/yihui/bookdown/ for how to compile this example into HTML. You may generate a copy of the book in `bookdown::pdf_book` format by calling `bookdown::render_book('index.Rmd', 'bookdown::pdf_book')`. More detailed instructions are available here https://bookdown.org/yihui/bookdown/build-the-book.html.

You can find the preview of this example at https://bookdown.org/yihui/bookdown-demo/.
