#' Plot frequency of common variables across data sets
#'
#' @param di_list a list of objects returned from \code{\link{get_data_info}}
#'
#' @export
#' @importFrom dplyr group_by summarise arrange filter
plot_common_vars_freq <- function(di_list) {
  allvars <- sort(unique(unname(unlist(lapply(di_list, function(x) names(x$var_summ))))))

  vars <- do.call(rbind, lapply(di_list, function(x) {
    res <- data.frame(variable = allvars, stringsAsFactors = FALSE)
    res$present <- 0
    res$present[res$variable %in% names(x$var_summ)] <- 1
    res
  }))
  vars$variable <- factor(vars$variable)

  vorder <- vars %>% dplyr::group_by(variable) %>%
    dplyr::summarise(n = sum(present)) %>%
    dplyr::arrange(-n)
  vorder <- vorder$variable

  vtab <- data.frame(xtabs(present ~ variable, data = vars))
  vtab2 <- subset(vtab, Freq > 1)
  vtab2$variable <- gsub(":", "_", vtab2$variable)

  rbokeh::figure(width = 800,
    xlim = vtab2$variable[rev(order(vtab2$Freq))]) %>%
    rbokeh::ly_points(variable, Freq, data = vtab2, hover = vtab2) %>%
    rbokeh::theme_axis("x", major_label_orientation = 60)
}

#' Plot dataset / variable pairs
#'
#' @param di_list a list of objects returned from \code{\link{get_data_info}}
#'
#' @export
#' @importFrom dplyr group_by summarise arrange
plot_common_vars_tab <- function(di_list) {
  datvars <- do.call(rbind, unname(lapply(di_list, function(x) {
    data.frame(id = x$id, variable = names(x$var_summ), stringsAsFactors = FALSE)
  })))

  tmp <- data.frame(datvars %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::arrange(-n) %>%
    dplyr::filter(n > 1))

  datvars <- datvars[datvars$variable %in% tmp$variable,]

  figure(height = 800, width = 800, xlim = tmp$variable) %>%
    ly_crect(variable, id, data = datvars, hover = datvars) %>%
    theme_axis("x", major_label_orientation = 90)
}

#' Plot number of rows in each data frame
#'
#' @param di_list a list of objects returned from \code{\link{get_data_info}}
#'
#' @export
plot_nrows <- function(di_list) {
  names(di_list) <- unname(unlist(lapply(di_list, function(x) x$id)))
  rows <- do.call(rbind, lapply(names(di_list), function(nm)
    data.frame(
      nrow = di_list[[nm]]$nrow,
      id   = nm,
      stringsAsFactors = FALSE)))

  rbokeh::figure(xlim = rows$id[order(rows$nrow)], width = 600) %>%
    rbokeh::ly_points(id, nrow, data = rows, hover = rows) %>%
    rbokeh::y_axis(log = 10) %>%
    rbokeh::theme_axis("x", major_label_orientation = 60)
}

#' Plot number of variables in each data frame
#'
#' @param di_list a list of objects returned from \code{\link{get_data_info}}
#'
#' @export
plot_nvars <- function(di_list) {
  names(di_list) <- unname(unlist(lapply(di_list, function(x) x$id)))
  vars <- do.call(rbind, lapply(names(di_list), function(nm)
    data.frame(
      nvar = length(di_list[[nm]]$var_summ),
      id   = nm,
      stringsAsFactors = FALSE)))

  rbokeh::figure(xlim = vars$id[order(vars$nvar)], width = 600) %>%
    rbokeh::ly_points(id, nvar, data = vars, hover = vars) %>%
    rbokeh::theme_axis("x", major_label_orientation = 60)
}
