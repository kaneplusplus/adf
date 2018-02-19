#' @title Default Method for Outputting ADF Calculations to Disk
#'
#' @description Default method for constructing output file to file system.
#' @param x an abstract data frame
#' @param FUN2 function to apply over each chunk
#' @param outDir an empty directory for storing output
#' @param params a list of additional parameters
#' @importFrom foreach foreach %dopar%
#' @importFrom iotools chunk.apply as.output chunk.reader
#' @export
default.output.function <- function (x, FUN2, outDir, params) {

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

  outMethod <- params$outMethod
  if (is.null(outMethod)) {
    outMethod <- "file"
  }
  if (outMethod == "file") {
    prefix <- ".csv"
  }
  else if (outMethod == "gzfile") {
    prefix <- ".gz"
  }
  else if (outMethod == "bzfile") {
    prefix <- ".bz2"
  }
  else if (outMethod == "xzfile") {
    prefix <- ".xz"
  }
  else {
    prefix <- ""
  }

  if (dir.exists(outDir)) {
    stop("outDir already exists")
  } else {
    dir.create(outDir, recursive = TRUE)
  }

  n <- length(x$createNewConnection)
  outFile <- sprintf("%s/file-%05d%s", outDir, 0:(n - 1), prefix)
  createNewConnection <- NULL

  foreach(createNewConnection = x$createNewConnection,
          outFile = outFile, .combine = combine) %dopar% {

    on.exit({
      close(conIn)
      close(conOut)
    })

    conIn <- eval(createNewConnection)
    conOut <- eval(call(outMethod, description = outFile, open = "wb"))

    FUN3 <- function(z) {
      r <- iotools::as.output(FUN2(z))
      writeBin(r, conOut)
    }

    if (x$skip > 0L) {
      readLines(conIn, n = x$skip)
    }

    cr <- chunk.reader(conIn, max.line = params$chunk.max.line)

    chunk.apply(cr, FUN3, CH.MERGE = params$CH.MERGE,
      CH.MAX.SIZE = params$CH.MAX.SIZE, parallel = 1L)
    NULL
  }
  outDir
}

