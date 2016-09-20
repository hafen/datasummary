## change character columns to numeric if it looks like they are
fix_character <- function(dat, max_levels = 100) {
  chg <- sapply(names(dat), function(nm) {
    if (is.character(dat[[nm]])) {
      # at least 50% unique values
      too_unique <- length(unique(dat[[nm]])) <= max_levels
      if (all(is.na(dat[[nm]])) || too_unique)
        return(FALSE)
      return(all(grepl("^-?(([0-9]*)|(([0-9]*)\\.([0-9]*)))$", dat[[nm]]) | is.na(dat[[nm]])))
    } else {
      return(FALSE)
    }
  })
  chg <- which(chg)
  if (length(chg) > 0) {
    for (ch in names(chg)) {
      message("   ", ch)
      dat[[ch]] <- as.numeric(dat[[ch]])
    }
  }
  dat
}
