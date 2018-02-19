context("adf.apply works")

n <- 100
test_df <- data.frame(col1 = sample(state.abb,n,TRUE),
                       col2 = sample(1:10,n,TRUE),
                       col3 = runif(n),
                       col4 = complex(n,runif(n),runif(n)),
                       stringsAsFactors = FALSE)
write.table(test_df, tf <- tempfile(), sep = "|",
             quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(test_df, tf2 <- tempfile(), sep = "|",
             quote = FALSE, row.names = FALSE, col.names = FALSE)

adfObj <- adf(c(tf,tf2))
adfObj <- allFactorLevels(adfObj)

test_that("We can perform linear regression with adf.apply.", {
  # Construct OLS beta hat

  adfObj <- adf(c(tf,tf2))
  calcOLSmats <- function(u) list(XtX = t(u$x) %*% u$x, Xty = t(u$x) %*% u$y)
  v <- adf.apply(adfObj, formula = "V3 ~ V2 + V1", calcOLSmats ,
                 type = "model")
  XtX <- Reduce(`+`, Map(getElement, v, "XtX"))
  Xty <- Reduce(`+`, Map(getElement, v, "Xty"))

  test_df2 <- rbind(test_df)
  betaDF <- coef(lm(col3 ~ col2 + col1, data = test_df2))
  betaADF <- qr.solve(XtX, Xty)
  err <- max(abs(betaDF - betaADF))
  expect_true( abs(err) < 1e-13 )
})

unlink(tf)
unlink(tf2)

