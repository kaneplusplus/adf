# Note you have to use the fork at kaneplusplus/aws.s3 until my pull
# request is merged.
library(aws.s3)
library(iotools)

context("adf.apply on minio")

# Connect to the public server.

# Set the key and secret key to the public minio server's.
Sys.setenv("AWS_ACCESS_KEY_ID" = "Q3AM3UQ867SPQQA43P2F",
           "AWS_SECRET_ACCESS_KEY" = "zuf+tfteSlswRu7BJ86wekitnifILbZam1KYY3TG")

# Use the public server for requests.
set_base_url("play.minio.io:9000")

# Create a bucket.
random_bucket <- paste(sample(letters, 10), collapse="")
put_bucket(random_bucket)


n <- 100
test_df <- data.frame(col1 = sample(state.abb,n,TRUE),
                      col2 = sample(1:10,n,TRUE),
                      col3 = runif(n),
                      col4 = complex(n,runif(n),runif(n)),
                      stringsAsFactors = FALSE)

# Put the test data into the bucket.
put_object(as.output(test_df), "part-1", random_bucket)
put_object(as.output(test_df), "part-2", random_bucket)

# Write these so that we have an adfObj to fix.
write.table(test_df, tf <- tempfile(), sep = "|",
             quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(test_df, tf2 <- tempfile(), sep = "|",
             quote = FALSE, row.names = FALSE, col.names = FALSE)

adfObjOld <- adf(c(tf,tf2))

# Now unlink the temporary files.
unlink(tf)
unlink(tf2)

# Now make it point to minio.
adfObjOld$createNewConnection[[1]] <- 
  parse(text="rawConnection(get_object('part-1', random_bucket))")
adfObjOld$createNewConnection[[2]] <- 
  parse(text="rawConnection(get_object('part-2', random_bucket))")

adfObj <- allFactorLevels(adfObjOld)

test_that("allFactorLevels is working.", {
  expect_equal(adfObj, allFactorLevels(adfObjOld))
})

test_that("We can perform linear regression with adf.apply.", {
  # Construct OLS beta hat

  calcOLSmats <- function(u) list(XtX = t(u$x) %*% u$x, Xty = t(u$x) %*% u$y)
  v <- adf.apply(adfObj, formula = "V3 ~ V2 + V1", calcOLSmats ,
                 type = "model")
  XtX <- Reduce(`+`, Map(getElement, v, "XtX"))
  Xty <- Reduce(`+`, Map(getElement, v, "Xty"))

  test_df2 <- rbind(test_df)
  betaDF <- coef(lm(col3 ~ col2 + col1, data = test_df2))
  betaADF <- qr.solve(XtX, Xty)
  err <- max(abs(betaDF - betaADF))
  expect_true( abs(err) < 1e-10 )
})

delete_object("part-1", random_bucket)
delete_object("part-2", random_bucket)
delete_bucket(random_bucket)


