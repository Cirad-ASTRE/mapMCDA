context("Detection of separator in csv files")

test_that("Detects various separators correctly", {

  tf <- tempfile()
  tf2 <- tempfile()
  
  ## Standard csv file with decimal point
  td <- data.frame(T = LETTERS[1:10], N = runif(10))
  write.csv(td, tf, row.names = FALSE)
  file.copy(tf, tf2)
  expect_identical(csv_sep(tf), ",")

  ## Separate with ";" and decimal comma
  writeLines(gsub(",", ";", readLines(tf)), file(tf2))
  writeLines(gsub("\\.", ",", readLines(tf2)), file(tf2))
  expect_identical(csv_sep(tf2), ";")
  close(file(tf2))

  ## Separate with ":"
  writeLines(gsub(",", ":", readLines(tf)), file(tf2))
  expect_identical(csv_sep(tf2), ":")
  close(file(tf2))

  ## Separate with " "
  writeLines(gsub(",", " ", readLines(tf)), file(tf2))
  expect_identical(csv_sep(tf2), " ")
  close(file(tf2))

  ## Separate with tab
  writeLines(gsub(",", "\t", readLines(tf)), file(tf2))
  expect_identical(csv_sep(tf2), "\t")
  close(file(tf2))

  ## Separate with "|"
  writeLines(gsub(",", "|", readLines(tf)), file(tf2))
  expect_identical(csv_sep(tf2), "|")
  close(file(tf2))

  close(file(tf))
})
