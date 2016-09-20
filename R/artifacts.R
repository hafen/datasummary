#' Get all summary data necessary for generatng a summary report
#'
#' @param dat a data frame
#' @param meta an optional data frame with up to three columns - the first, "name" provides the name of the variable, the second "label" provides a label for the variable, and the optional third, "group" allows a grouping of the variables in the data for presentation purposes
#' @param id optional id to use
#' @param \ldots optional additional attributes to store with the returned object (such as additional meta data)
#' @export
#' @importFrom digest digest
get_data_info <- function(dat, meta = NULL, id = digest::digest(dat), ...) {
  cd <- sapply(dat, digest::digest)
  na_cols <- which(sapply(dat, function(x) all(is.na(x))))
  if (length(na_cols) > 0)
    cd <- cd[-na_cols]
  cd_tab <- data.frame(table(cd), stringsAsFactors = FALSE)
  cd_tab <- cd_tab[cd_tab$Freq > 1, ]

  dups <- lapply(cd_tab$cd, function(a) {
    names(cd)[cd == a]
  })

  res <- structure(list(
    id         = id,
    col_digest = cd,
    na_cols    = na_cols,
    dups       = dups,
    head       = head(dat),
    nrow       = nrow(dat),
    ncol       = ncol(dat),
    var_summ   = get_var_summ(dat[setdiff(seq_len(ncol(dat)), na_cols)], meta = meta),
    ...
  ), class = c("data_info", "list"))

  add_var_artifacts(res)
}

## internal
##---------------------------------------------------------

#' @importFrom DistributionUtils skewness
get_var_summ <- function(dat, meta = NULL) {
  structure(lapply(names(dat), function(nm) {
    x <- dat[[nm]]
    label <- NULL
    group <- NULL
    if (is.data.frame(meta) && "name" %in% names(meta)) {
      idx <- which(meta$name == nm)
      if (length(idx) > 0) {
        if ("label" %in% names(meta))
          label <- meta$label[idx[1]]
        if ("group" %in% names(meta))
          group <- meta$group[idx[1]]
      }
    }
    if (is.factor(x))
      x <- as.character(x)
    if (is.character(x)) {
      truncated <- FALSE
      lvls <- data.frame(table(x))
      lvls$x <- as.character(lvls$x)
      lvls <- lvls[rev(order(lvls$Freq)), ]
      if (nrow(lvls) > 100)
        truncated <- TRUE
      return(list(
        type = "character",
        lvls = head(lvls, 100),
        nna = length(which(is.na(x))),
        nunique = nrow(lvls),
        truncated = truncated,
        name = nm,
        label = label,
        group = group
      ))
    } else if (is.numeric(x)) {
      n0 <- length(which(x == 0))
      skw <- DistributionUtils::skewness(x, na.rm = TRUE)
      log <- FALSE
      if (!is.nan(skw) && skw > 1.5 && all(x >= 0, na.rm = TRUE)) {
        log <- TRUE
        x <- x[x > 0]
        x2 <- log10(x)
        rng <- range(x2, na.rm = TRUE)
        brks <- 10 ^ seq(rng[1], rng[2], length = nclass.Sturges(x))
        hst <- hist(x, breaks = brks, plot = FALSE)
      } else {
        hst <- hist(x, plot = FALSE)
      }
      x2 <- x[!is.na(x)]
      qnn <- min(length(x2), 100)
      qnt <- data.frame(x = ppoints(qnn), y = quantile(x2, ppoints(qnn)))
      return(list(
        log = log,
        n0 = n0,
        type = "numeric",
        summ = summary(x),
        hist = hst,
        qnt = qnt,
        nunique = length(unique(x)),
        nna = length(which(is.na(x))),
        name = nm,
        label = label,
        group = group
      ))
    } else {
      return(list(
        type = class(x),
        nna = length(which(is.na(x))),
        name = nm,
        label = label,
        group = group
      ))
    }
  }
  ), names = names(dat))
}

#' @importFrom knitr kable
#' @importFrom DT datatable
add_var_artifacts <- function(di) {
  nms <- names(di$var_summ)
  for (ii in seq_along(di$var_summ)) {
    message(ii)
    vr <- di$var_summ[[ii]]
    di$var_summ[[ii]]$artifacts <- list()
    if (vr$type == "numeric") {
      fg <- plot_num(vr$hist, vr$qnt, xlab = nms[ii], log = vr$log)
      tb <- knitr::kable(data.frame(statistic = names(vr$summ), value = as.vector(vr$summ)))

      di$var_summ[[ii]]$artifacts$tb <- tb
      di$var_summ[[ii]]$artifacts$fg <- fg

    } else if (vr$type == "character") {
      names(vr$lvls)[1] <- "variable"

      fg <- plot_cat(head(vr$lvls, 50))
      dt <- DT::datatable(vr$lvls, rownames = FALSE)

      di$var_summ[[ii]]$artifacts$tb <- dt
      di$var_summ[[ii]]$artifacts$fg <- fg
    } else {
      di$var_summ[[ii]]$artifacts
    }
  }
  di
}
