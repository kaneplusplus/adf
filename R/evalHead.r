
evalHead <- function (createCon, chunkProcessor, sep, skip, header, strict,
                      colNames, nrowsClasses) {
    f <- eval(createCon)
    readLines(f, skip)
    if (header) {
        headerRow <- readLines(f, 1L)
        colNames <- strsplit(headerRow, split = sep, fixed = TRUE)[[1]]
    }
    r <- readLines(f, nrowsClasses)
    close(f)
    m <- iotools::mstrsplit(r, sep = sep, strict = strict, type = "character")
    m <- chunkProcessor(m)
    colClasses = apply(m, 2, function(v) class(type.convert(v,
        as.is = TRUE)))
    colClasses[colClasses == "factor"] = "character"
    colClasses[colClasses == "logical"] = "character"
    if (is.null(colNames))
        colNames <- paste0("V", 1:ncol(m))
    names(colClasses) <- colNames
    colClasses
}

