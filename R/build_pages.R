#' Build summary pages for a set of data sets with an optional index page
#'
#' @param di_list a list of objects returned from \code{\link{get_data_info}} or named list of data frames
#' @param title page title
#' @param author name of author
#' @param output_dir directory where output html files should be placed
#' @param index should an index page be built as well?
#' @param intro_rmd passed on to \code{\link{build_index_page}}
#' @param view should the index page be opened in a browser after it is rendered?
#'
#' @export
build_pages <- function(di_list, title = "", author = "", output_dir, index = TRUE, intro_rmd = "", view = FALSE) {

  output_dir <- check_output(output_dir)

  di_list <- check_di_list(di_list)

  ids <- unname(unlist(lapply(di_list, function(x) x$id)))

  for(ii in seq_along(di_list)) {
    message("* building ", di_list[[ii]]$id)
    title1 <- paste0(title, ": ", di_list[[ii]]$id)
    build_data_page(di_list[[ii]], title = title1, author = author,
      output_dir = output_dir, ids = ids)
  }
  if(index)
    build_index_page(di_list, title = title, author = author,
      output_dir = output_dir, intro_rmd = intro_rmd)

  mfpath <- file.path(system.file(package = "datasummary"),
    "templates/menu-fix.css")
  file.copy(mfpath, file.path(output_dir, "assets/"), overwrite = TRUE)

  ff <- file.path(output_dir, paste0("index.html"))
  if(view && file.exists(ff))
    browseURL(ff)

  invisible(TRUE)
}

#' Build a summary page for a data set
#'
#' @param di an object returned from \code{\link{get_data_info}} or data frame
#' @param title page title
#' @param author name of author
#' @param output_dir directory where output html file should be placed
#' @param ids an optional list of ids to be used to generate links to pages for other data sets
#' @param view should the page be opened in a browser after it is rendered?
#'
#' @export
#' @importFrom whisker whisker.render
#' @importFrom rmarkdown render
#' @importFrom packagedocs package_docs
build_data_page <- function(di, title = "", author = "", output_dir, ids = NULL, view = FALSE) {

  if(is.data.frame(di))
    di <- get_data_info(di)

  output_dir <- check_output(output_dir)

  navpills <- get_navpills(ids, cur_id = di$id)

  ff <- tempfile()
  save(di, file = ff)

  tmpldat <- list(
    title = title,
    author = author,
    navpills = navpills,
    di_path = ff,
    var_summaries = get_var_summary_sections(di)
  )

  tmpl_path <- file.path(system.file(package = "datasummary"),
    "templates/data_template.Rmd")
  tmpl <- readLines(tmpl_path)
  page <- whisker::whisker.render(tmpl, tmpldat)

  ff <- paste0(di$id, ".Rmd")
  cat(page, file = file.path(output_dir, ff))

  rmarkdown::render(file.path(output_dir, ff),
    output_format = packagedocs::package_docs(lib_dir = file.path(output_dir, "assets"),
      css = "assets/menu-fix.css"),
    output_dir = output_dir)

  mfpath <- file.path(system.file(package = "datasummary"),
    "templates/menu-fix.css")
  file.copy(mfpath, file.path(output_dir, "assets/"), overwrite = TRUE)

  if(view)
    browseURL(file.path(output_dir, paste0(di$id, ".html")))

  invisible(TRUE)
}

#' Build index pages for a set of data sets
#'
#' @param di_list a list of objects returned from \code{\link{get_data_info}} or named list of data frames
#' @param title page title
#' @param author name of author
#' @param output_dir directory where output html files should be placed
#' @param intro_rmd optional R markdown string that will be placed at beginning of the page
#' @param \ldots data to be saved and loaded in to the document's knitr environment
#'
#' @export
build_index_page <- function(di_list, title = "", author = "", output_dir, intro_rmd = "", ...) {

  di_list <- check_di_list(di_list)

  ids <- unname(unlist(lapply(di_list, function(x) x$id)))

  output_dir <- check_output(output_dir)

  navpills <- get_navpills(ids)

  ff <- tempfile()
  save(di_list, file = ff)

  eff <- tempfile()
  dots <- list(...)
  save(dots, file = eff)

  tmpldat <- list(
    title = title,
    author = author,
    navpills = navpills,
    di_list_path = ff,
    extra_path = eff,
    intro_rmd = intro_rmd
  )

  tmpl_path <- file.path(system.file(package = "datasummary"),
    "templates/index_template.Rmd")
  tmpl <- readLines(tmpl_path)
  page <- whisker::whisker.render(tmpl, tmpldat)

  pf <- file.path(output_dir, "index.Rmd")
  cat(page, file = pf)

  rmarkdown::render(pf,
    output_format = packagedocs::package_docs(lib_dir = file.path(output_dir, "assets"),
      css = "assets/menu-fix.css"),
    output_dir = output_dir)

  mfpath <- file.path(system.file(package = "datasummary"),
    "templates/menu-fix.css")
  file.copy(mfpath, file.path(output_dir, "assets/"), overwrite = TRUE)

  invisible(TRUE)
}

## internal
##---------------------------------------------------------

check_output <- function(output_dir) {
  output_dir <- normalizePath(output_dir)
  assets_dir <- file.path(output_dir, "assets")
  if(!file.exists(output_dir))
    dir.create(output_dir)
  if(!file.exists(assets_dir))
    dir.create(assets_dir)

  output_dir
}

check_di_list <- function(di_list) {
  if(!inherits(di_list, "data_info") && is.list(di_list) && all(sapply(di_list, is.data.frame))) {
    if(is.null(names(di_list)))
      stop("di_list must either be a named list of data frames or a list of objects from get_data_info()", call. = FALSE)

    return(lapply(names(di_list), function(nm) {
      get_data_info(di_list[[nm]], id = nm)
    }))
  } else {
    return(di_list)
  }
}

get_navpills <- function(ids, cur_id = NULL) {
  if(is.null(ids))
    return("")

  if(is.null(cur_id)) {
    cur_id <- "*&^%%#&^%*()(*&%$%#@)"
    pid <- ""
    pactive = "active"
  } else {
    pid <- paste0("(", cur_id, ")")
    pactive = ""
  }

  tmpldat <- list(
    id = pid,
    active = pactive,
    dd_active = ifelse(pactive == "", "active", ""),
    navpills = lapply(ids, function(id) {
      list(id = id,
        active = ifelse(cur_id == id, "active", ""))
    }))

  tmpl_path <- system.file(package = "datasummary",
    "templates/navpills_template.yaml")
  tmpl <- readLines(tmpl_path)
  paste0("\n", whisker::whisker.render(tmpl, tmpldat))
}


get_var_summary_sections <- function(di) {
  a <- sapply(di$var_summ, get_var_summary_section)
  paste(a, collapse = "\n")
}

get_var_summary_section <- function(vr) {
  if(vr$type == "character") {
    header <- paste0("### Distribution",
      ifelse(vr$truncated, " of top 50 variables", ""), " ###")
    txt <- c(header, "",
      ifelse(vr$log, paste0("Due to high skewness, the plot below is shown with the variable transformed to the log scale.  There were ", vr$n0, " zeros removed prior to transformation."), ""),
      "```{r, echo=FALSE, message=FALSE, results='asis'}",
      paste0("vr <- di$var_summ[[\"", vr$name, "\"]]"),
      "vr$artifacts$fg", "```", "")

    header <- paste0("### Frequency table",
      ifelse(vr$truncated, " of top 100 variables", ""), " ###")
    txt <- c(txt, header, "",
      "```{r, echo=FALSE, message=FALSE, results='asis'}",
      "vr$artifacts$tb",
      "```", "")
  } else if(vr$type == "numeric") {
    txt <- c("### Distribution ###", "",
      "```{r, echo=FALSE, message=FALSE, results='asis'}",
      paste0("vr <- di$var_summ[[\"", vr$name, "\"]]"),
      "vr$artifacts$fg", "```", "")

    txt <- c(txt, "### Summary statistics ###", "",
      "```{r, echo=FALSE, message=FALSE, results='asis'}",
      "vr$artifacts$tb",
      "```", "")
  } else {
    txt <- ""
  }
  if(length(txt) > 1)
    txt <- c(paste("##", vr$name, "##"), "", txt)

  paste(txt, collapse = "\n")
}
