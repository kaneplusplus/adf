
#' @title Get All Factor Levels of a (Abstract) Data Frame
#' 
#' @description Get all factor levels of a (possibly abstract) data frame.
#' @param x (abstract) data frame
#' @export
allFactorLevels <- function(x) {
  UseMethod("allFactorLevels")
}

#' @export
allFactorLevels.default <- function(x) {
  stop(paste0("Don't know how to get factor levels for an object of type ",
    class(x), "."))
}

#' @export
allFactorLevels.data.frame <- function(x) {
  stop("We really should implement this.")
}

#' @export
allFactorLevels.adf <- function (x) {

  if (!inherits(x, "adf")) {
    stop("x must be an 'adf' object!")
  }

  index <- which(x$colClasses == "character")

  if (length(index) == 0L) {
    return(x)
  }

  x$levels <- vector(mode = "list", length = length(index))
  names(x$levels) <- names(x$colClasses)[index]

  z <- adf.apply(x, FUN = function(d) lapply(d[, index, drop = FALSE], unique))

  for (j in 1:length(index)) {
    x$levels[[j]] <- sort(unique(
      unlist(Map(function(w) w[j], z), use.names = FALSE)))
  }
  x
}

