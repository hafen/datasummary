globalVariables(c("hst", "variable", "count", "type", "Freq", "value2", "value", "variable", "present", "n", "Freq", "id", "nvar"))

#' Plot a categorical frequency table
#'
#' @param dtab a frequency table data frame
#' @param width width of the plot in pixels
#' @param min_height minimum height of the plot in pixels (the actual height is computed as a function of the number of rows in \code{dtab})
#'
#' @export
#' @import rbokeh
plot_cat <- function(dtab, width = 750, min_height = 400) {
  dtab$value <- as.character(dtab$variable)
  nc <- nchar(dtab$value)
  cutoff <- ifelse(nc > 20, 17, 20)
  dtab$value2 <- substr(dtab$value, 1, cutoff)
  dtab$value2[cutoff < 20] <- paste0(dtab$value2[cutoff < 20], "...")
  dtab$value2 <- paste(seq_along(dtab$value2), dtab$value2, sep = "- ")
  # workaround because bokeh doesn't like ":" in axis labels
  dtab$value2 <- gsub(":", "_", dtab$value2)
  rbokeh::figure(ylim = rev(dtab$value2), ylab = NULL,
    width = width, height = max(min_height, 100 + 12 * nrow(dtab))) %>%
    rbokeh::ly_points(Freq, value2, hover = list(value, Freq), data = dtab)
}

#' Plot a histogram for a numeric variable
#'
#' @param hst an object returned from \code{\link[graphics]{hist}}
#' @param xlab x-axis label
#' @param log should the x-axis be shown on the log scale?
#' @param width width of the plot in pixels
#' @param height height of the plot in pixels
#'
#' @export
plot_num <- function(hst, xlab = "x", log = FALSE, width = 750, height = 520) {
  fg <- figure(xlab = xlab, width = 750) %>% ly_hist(hst)
  if(log)
    fg <- fg %>% x_axis(log = TRUE)
  fg
}

#' Plot number of missing values for each variable in a data frame
#'
#' @param di an object returned from \code{\link{get_data_info}}
#' @param width width of the plot in pixels
#' @param height height of the plot in pixels
#' @param \ldots additional parameters passed to \code{\link[rbokeh]{figure}}
#'
#' @export
plot_missing <- function(di, width = 800, height = 600, ...) {
  nn <- di$nrow
  nms <- names(di$var_summ)
  nna <- unname(sapply(di$var_summ, function(x) x$nna))

  tab <- rbind(
    data.frame(variable = nms, count = nna, type = "NA", stringsAsFactors = FALSE),
    data.frame(variable = nms, count = nn - nna, type = "non-NA", stringsAsFactors = FALSE)
  )

  rbokeh::figure(width = width, height = height, ...) %>%
    rbokeh::ly_bar(variable, count, color = type, data = tab, width = 1) %>%
    rbokeh::theme_axis("x", major_label_orientation = 60) %>%
    rbokeh::theme_grid("x", grid_line_alpha = 0.3)
}

