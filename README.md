# data summary

This R package creates rmarkdown data summary reports for a data frame or list of data frames.  It is the result an effort that came out of a DARPA XDATA hackathon for exploring and merging a collection of building permit data sets collected from around the web.

## Install

```s
options(repos = c(tessera = "http://packages.tessera.io", getOption("repos")))
install.packages("datasummary")
```

## Use

Some examples:

```s
library(datasummary)

# summary of a single data set
build_data_page(iris, title = "iris data",
  output_dir = file.path(tempdir(), "test1"), view = TRUE)

# summary of multiple data sets
iris2 <- data.frame(iris, newvar = rnorm(150))
build_pages(list(iris = iris, iris2 = iris2), title = "iris data",
  output_dir = file.path(tempdir(), "test2"), view = TRUE)
```

