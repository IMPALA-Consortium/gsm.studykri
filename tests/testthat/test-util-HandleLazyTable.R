test_that("GenerateUniqueTempName creates unique names", {
  name1 <- GenerateUniqueTempName("test")
  name2 <- GenerateUniqueTempName("test")
  
  # Should be different
  expect_false(name1 == name2)
  
  # Should follow pattern: base_pid_random
  expect_true(grepl("^test_\\d+_\\d+$", name1))
  expect_true(grepl("^test_\\d+_\\d+$", name2))
  
  # Should start with base name
  expect_true(startsWith(name1, "test_"))
  expect_true(startsWith(name2, "test_"))
})

test_that("GenerateUniqueTempName works with different base names", {
  bootstrap_name <- GenerateUniqueTempName("bootstrap_reps")
  month_name <- GenerateUniqueTempName("month_sequences")
  
  expect_true(startsWith(bootstrap_name, "bootstrap_reps_"))
  expect_true(startsWith(month_name, "month_sequences_"))
  expect_false(bootstrap_name == month_name)
})

test_that("GenerateUniqueTempName handles special characters in base name", {
  # Underscores and numbers should work fine
  name <- GenerateUniqueTempName("temp_table_123")
  expect_true(startsWith(name, "temp_table_123_"))
  expect_true(grepl("^temp_table_123_\\d+_\\d+$", name))
})
