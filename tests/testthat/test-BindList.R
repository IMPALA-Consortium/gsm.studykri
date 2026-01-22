test_that("test BindLists drops argument names passed to ...", {
  ls <- BindLists(
    A = list(a = 1, b = 2),
    B = list(c = 3, d = 4),
    C = list(e = 5, f = 6)
  )

  expect_equal(names(ls), letters[1:6])
})
