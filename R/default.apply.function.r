
#' @title Default Method for Applying a Function to an Abstrct Data Frame
#'
#' @description Default method for applying a function to an abstract
#' data frame.
#' @param x an abstract data frame
#' @param FUN2 function to apply over each chunk of the abstract data frame
#' @param params a list of additional parameters
#' @importFrom foreach %dopar% foreach
#' @importFrom iotools chunk.apply chunk.reader
#' @export
default.apply.function <- function (x, FUN2, params) {

    if (is.null(params$chunk.max.line)) {
        params$chunk.max.line <- 65536L
    }
    if (is.null(params$CH.MAX.SIZE)) {
        params$CH.MAX.SIZE <- 33554432L
    }
    if (is.null(params$CH.MERGE)) {
        params$CH.MERGE <- NULL
    }
    if (is.null(params$chunk.max.line)) {
        params$chunk.max.line <- 65536L
    }
    if (is.null(params$parallel)) {
        params$parallel <- 1L
    }
    if (is.null(params$CH.MERGE)) {
        combine <- c
        params$CH.MERGE <- list
    } else {
      combine = params$CH.MERGE
    }

    createNewConnection <- NULL
    foreach (createNewConnection = x$createNewConnection,
             .combine = combine, .packages = "iotools") %dopar% {

        on.exit(close(con))
        con <- eval(createNewConnection)

        if (x$skip > 0L) {
            readLines(con, n = x$skip)
        }

        cr <- chunk.reader(con, max.line = params$chunk.max.line)

        chunk.apply(cr, FUN2, CH.MERGE = params$CH.MERGE,
                    CH.MAX.SIZE = params$CH.MAX.SIZE, 
                    parallel = params$parallel)
    }
}

