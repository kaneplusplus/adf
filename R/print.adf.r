print.adf <- function (x, ...) {
    w = min(max(nchar(x$colNames), 10L), 40L)
    cat(sprintf("    An abstract data frame with %d columns:\n\n",
        length(x$colClasses)))
    cat(sprintf(paste0("    %-", w, "s  %-10s"), names(x$colClasses),
        x$colClasses), sep = "\n")
}

