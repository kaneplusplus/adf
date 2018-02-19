
#' @title Apply a Function to an Abstract Data Frame
#'
#' @description Low level function for applying over an abstract data frame.
#'
#' @param x an abstract data frame object
#' @param FUN function to apply over each chunk; its first argument must
#'   accept the abstract data frame, and the second (optional)
#'   argument accepts the args parameter
#' @param args Option list of arguments which are passed as a second
#'   argument to FUN
#' @param outDir if 'NULL', the default, results are passed back to R;
#'   otherwise this gives the output location (a new directory)
#'   for storing the results
#' @param type type of data to give as an input to FUN. If model or sparse
#'   model, this is a list giving the response (y), model matrix
#'   (x), weights (w), and offset (offset) from the input forumal.
#' @param formula a formula to used with type equal to model or sparse.model
#' @param contrasts contrasts to used with type equal to model or sparse.model
#' @param subset a string to to used with type equal to model or sparse.model.
#'   Will be evaluated in the environment of the data frame (ex.
#'   subset = "V2 + V3 > V4")
#' @param weights a string to to used with type equal to model or sparse.model.
#'   Will be evaluated in the environment of the data frame.
#' @param na.action a function which indicates what should happen when the data
#'   contain 'NA's. See lm.fit for more details.
#' @param offset a string to to used with type equal to model or sparse.model.
#'   Will be evaluated in the environment of the data frame.
#' @param params a named list of additional parameters that depends on the
#'   type of abstract data frame that was created
#' @examples
#' n <- 100
#' test_df <- data.frame(col1 = sample(state.abb,n,TRUE),
#'                       col2 = sample(1:10,n,TRUE),
#'                       col3 = runif(n),
#'                       col4 = complex(n,runif(n),runif(n)),
#'                       stringsAsFactors = FALSE)
#' write.table(test_df, tf <- tempfile(), sep = "|",
#'             quote = FALSE, row.names = FALSE, col.names = FALSE)
#' write.table(test_df, tf2 <- tempfile(), sep = "|",
#'             quote = FALSE, row.names = FALSE, col.names = FALSE)
#' 
#' adfObj <- adf(c(tf,tf2))
#' adfObj <- allFactorLevels(adfObj)
#' 
#' # Construct OLS beta hat
#' adfObj <- adf(c(tf,tf2))
#' calcOLSmats <- function(u) list(XtX = t(u$x) %*% u$x, Xty = t(u$x) %*% u$y)
#' v <- adf.apply(adfObj, formula = "V3 ~ V2 + V1", calcOLSmats , 
#'                type = "model")
#' XtX <- Reduce(`+`, Map(getElement, v, "XtX"))
#' Xty <- Reduce(`+`, Map(getElement, v, "Xty"))
#' 
#' test_df2 <- rbind(test_df)
#' betaDF <- coef(lm(col3 ~ col2 + col1, data = test_df2))
#' betaADF <- qr.solve(XtX, Xty)
#' err <- max(abs(betaDF - betaADF))
#' err
#' 
#' unlink(tf)
#' unlink(tf2)
#' @importFrom foreach getDoParName registerDoSEQ
#' @importFrom Matrix sparse.model.matrix
#' @importFrom stats model.frame model.matrix model.offset model.response
#' @importFrom stats model.weights
#' @export
adf.apply <- function(x, FUN, args = list(), outDir = NULL,   
  type = c("data.frame", "model", "sparse.model"), formula = NULL, 
  contrasts = NULL, subset = NULL, weights = NULL, na.action = NULL, 
  offset = NULL, params = list()) {

  if (!inherits(x, "adf")) {
    stop("x must be an 'adf' object!")
  }

  type <- match.arg(type)
  fm <- formals(FUN)
  arg2flag <- TRUE
  
  if (is.null(fm) || length(fm) < 2L) {
    arg2flag <- FALSE
  }

  if (is.null(formula) & type != "data.frame") {
    stop("model and sparse.model require a formula!")
  }

  FUN2 <- function(z) {
    df <- x$chunkFormatter(z, x$colClasses, x$levels)
    df <- x$chunkProcessor(df)
    if (type == "data.frame") {
      if (arg2flag) {
        return(FUN(df, args))
      } else {
        return(FUN(df))
      }
    }

    if (!is.null(subset)) {
      subset = eval(parse(text = paste0("with(df, ", subset, ")")))
      df = df[subset, ]
    }

    IO_OFFSET <- IO_WEIGHTS <- NULL

    environment(formula) <- parent.frame()

    if (is.null(weights) && is.null(offset)) {
      mf <- model.frame(formula = formula, data = df)
    } else if (is.null(weights)) {
      df$IO_OFFSET <- eval(parse(text = paste0("with(df, ", offset, ")")))
      mf <- model.frame(formula = formula, data = df, offset = IO_OFFSET)
    } else if (is.null(offset)) {
      df$IO_WEIGHTS <- eval(parse(text = paste0("with(df, ", weights, ")")))
      mf <- model.frame(formula = formula, data = df, weights = IO_WEIGHTS)
    } else {
      df$IO_OFFSET <- eval(parse(text = paste0("with(df, ", offset, ")")))
      df$IO_WEIGHTS <- eval(parse(text = paste0("with(df, ", weights, ")")))
      mf <- model.frame(formula = formula, data = df, offset = IO_OFFSET,
                        weights = IO_WEIGHTS)
    }

    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    offset <- as.vector(model.offset(mf))
    if (type == "sparse.model") {
      x <- sparse.model.matrix(mt, mf, contrasts.arg = contrasts)
    } else {
      x <- model.matrix(mt, mf, contrasts.arg = contrasts)
    }
  
    if (arg2flag) {
      FUN(list(y = y, x = x, w = w, offset = offset, mt = mt), args)
    } else {
      FUN(list(y = y, x = x, w = w, offset = offset, mt = mt))
    }
  }

  if (is.null(getDoParName())) {
    registerDoSEQ()
  }

  if (is.null(outDir)) {
    x$methods$apply(x, FUN2, params)
  } else {
    x$methods$output(x, FUN2, outDir, params)
  }
}

