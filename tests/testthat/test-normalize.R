test_that("prevents normalize character", {
  expect_error(
    normalize(LETTERS),
    "cannot work with objects of type 'character'"
  )
  expect_error(
    normalize(matrix(LETTERS, nrow = 2)),
    "cannot work with objects of type 'character'"
  )
})

test_that("can normalize single numeric", {
  expect_identical(
    normalize(1),
    structure(NA_real_, center = 1, scale = NA_real_)
  )
  expect_identical(
    normalize(1, center = FALSE),
    structure(NA_real_, scale = NA_real_)
  )
  expect_identical(
    normalize(1, scale = FALSE),
    structure(0, center = 1)
  )
  expect_identical(
    normalize(1, center = FALSE, scale = FALSE),
    1
  )
})

test_that("can normalize vector", {
  normalize(1)
  normalize(1, center = FALSE)
  normalize(1, scale = FALSE)
  normalize(1, center = FALSE, scale = FALSE)
})

test_that("can normalize matrix", {

})

test_that("can normalize data.frame", {

})

test_that("can normalize list", {

})
