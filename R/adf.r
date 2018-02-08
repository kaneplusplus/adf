
#' @title Create an abstract data frame
#'
#' @description Create an abstract data frame
#' @param description character string.  A description of the connection; the
#'   path to a file for file connections.
#' @param conMethod string indicating the connection method.
#' @param encoding encoding to use in the connections
#' @param expand.path logical. Should ‘description’ be normalized and wildcard
#'   expanded.
#' @param colClasses an optional character vector of column classes. If named
#'   and colNames is missing, the names will be used for colNames.
#'   If missing, will be automatically determined.
#' @param colNames an optional character vector of column names. If missing and
#'   header = TRUE, these will be determined be the first row of
#'   data. Otherwise, names will be constructed by pasting the
#'   character 'V' with the column number.
#' @param skip number of lines to strip off of the file or connection before
#'   parsing
#' @param chunkProcessor a function to apply post-processing to a formatted
#'   dataframe. Usually only used without a chunkFormatter.
#' @param chunkFormatter an optional function turning the raw connection into a
#'   dataframe. It must accept four parameters: data, colNames,
#'   colClasses, levels. If missing this will be constructed
#'   automatically.
#' @param sep character seperating the data columns. Ignored when
#'   chunkFormatter is given.
#' @param strict logical. Whether the parser should run in strict mode.
#'   Ignored when chunkFormatter is given.
#' @param header logical indicating whether the first line of data, after skip
#'   if provided, contains variable names. Ignored if
#'   chunkFormatter is also provided.
#' @param levels a named list, with names corresponding to the colNames of
#'   class character (or factor). Each element gives the levels
#'   for the corresponding variable. These will be automatically
#'   determined when missing.
#' @param nrowsClasses number of rows to pull for determining colClasses and
#'   factor levels when needed; will only be grabbed from the
#'   first file or connection if multiple are passed.
#' @return An abstract data frame object.
#' @examples
#'     n <- 100
#'     test_df <- data.frame(col1 = sample(state.abb,n,TRUE),
#'                           col2 = sample(1:10,n,TRUE),
#'                           col3 = runif(n),
#'                           col4 = complex(n,runif(n),runif(n)),
#'                           stringsAsFactors = FALSE)
#'     write.table(test_df, tf <- tempfile(), sep = "|",
#'                 quote = FALSE, row.names = FALSE, col.names = FALSE)
#'     write.table(test_df, tf2 <- tempfile(), sep = "|",
#'                 quote = FALSE, row.names = FALSE, col.names = FALSE)
#'
#'     adfObj <- adf(c(tf,tf2))
#'
#'     unlink(tf)
#'     unlink(tf2)
#' @export
adf <- function(description = "", conMethod = "file", 
        encoding = getOption("encoding"), expand.path = FALSE, 
        colClasses = NULL, colNames = NULL,
        skip = 0L, chunkProcessor = identity, chunkFormatter = NULL,
        sep = "|", strict = TRUE, header = FALSE, levels = list(),
        nrowsClasses = 250L) {

  if (expand.path) {
    description <- Sys.glob(path.expand(description))
  }

  if (!is.null(colClasses) & is.null(colNames)) {
    colNames <- names(colNames)
  }
  
  if (header && !missing(chunkFormatter)) {
    stop("Header cannot be used with user-defined chunkFormatter")
  }

  n <- length(description)
  encoding <- rep(encoding, n)
  conMethod <- rep(conMethod, n)
  openType <- rep("rb", n)

  createNewConnection <- mapply(call, conMethod, description = description,
    encoding = encoding, open = openType)

  if (is.null(names(colClasses))) {
    colClasses <- evalHead(createCon = createNewConnection[[1]],
      chunkProcessor = chunkProcessor, sep = sep, skip = skip,
      header = header, strict = strict, colNames = colNames,
      nrowsClasses = nrowsClasses)
    skip <- skip + header
  }

  if (is.null(chunkFormatter)) {
    chunkFormatter <- default.chunkFormatter(sep = sep, strict = strict)
  }
  
  structure(
    list(createNewConnection = createNewConnection,
        methods = list(apply = default.apply.function, 
                       output = default.output.function),
        chunkFormatter = chunkFormatter, 
        chunkProcessor = chunkProcessor,
        colClasses = colClasses, 
        levels = levels, 
        skip = skip),
        class = "adf", 
        package = "adf")
}
