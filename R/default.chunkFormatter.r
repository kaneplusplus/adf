
default.chunkFormatter <- function (sep = sep, strict = strict) {
    function(data, colClasses, levels) {
        col_types = colClasses
        colNames <- names(col_types)
        if (is.list(data))
            data = as.vector(data, mode = "character")
        z = iotools::dstrsplit(data, col_types = col_types, sep = sep,
            strict = strict)
        for (j in which(colClasses == "character")) {
            if (!is.null(levels[[colNames[j]]]))
                z[, j] = factor(z[, j], levels = levels[[colNames[j]]])
        }
        z
    }
}

