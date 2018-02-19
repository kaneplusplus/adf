lm.model.frame <- function(formula, data, subset, weights, na.action, 
                           contrasts = NULL, offset) {
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action",
        "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- FALSE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    return(mf)
}

