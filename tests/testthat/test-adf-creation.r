library(testthat)

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

context("adf creation")

test_that("We can create an adf from text files.", {
  adfObj <- adf(c(tf,tf2))
  cc <- c("character", "integer", "numeric", "complex")
  names(cc) <- paste0("V", 1:4)
  expect_equal(cc, adfObj$colClasses)
  expect_equal(list(), adfObj$levels)
  expect_equal(0, adfObj$skip)
})


unlink(tf)
unlink(tf2)

