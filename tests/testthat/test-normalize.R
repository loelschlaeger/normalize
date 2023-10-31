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
  x <- 1:10
  mean_x <- mean(x)
  sd_x <- stats::sd(x)
  expect_identical(
    normalize(x),
    structure((x - mean_x) / sd_x, center = mean_x, scale = sd_x)
  )
  expect_identical(
    normalize(x, center = FALSE),
    structure(x / sd_x, scale = sd_x)
  )
  expect_identical(
    normalize(x, scale = FALSE),
    structure(x - mean_x, center = mean_x)
  )
  expect_identical(
    normalize(x, center = FALSE, scale = FALSE),
    x
  )
})

test_that("can normalize matrix column-wise", {
  x <- matrix(1:12, nrow = 3, ncol = 4)
  expect_identical(
    normalize(x),
    structure(
      c(-1, 0, 1, -1, 0, 1, -1, 0, 1, -1, 0, 1),
      dim = 3:4,
      center = c(2, 5, 8, 11),
      scale = c(1, 1, 1, 1)
    )
  )
  expect_identical(
    normalize(x, center = FALSE),
    structure(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
      dim = 3:4,
      scale = c(1, 1, 1, 1)
    )
  )
  expect_identical(
    normalize(x, scale = FALSE),
    structure(
      c(-1, 0, 1, -1, 0, 1, -1, 0, 1, -1, 0, 1),
      dim = 3:4,
      center = c(2, 5, 8, 11)
    )
  )
  expect_identical(
    normalize(x, center = FALSE, scale = FALSE),
    structure(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
      dim = 3:4
    )
  )
})

test_that("can normalize matrix row-wise", {
  x <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
  expect_identical(
    normalize(x, byrow = TRUE),
    structure(
      c(-1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1),
      dim = 4:3,
      center = c(2, 5, 8, 11),
      scale = c(1, 1, 1, 1)
    )
  )
  expect_identical(
    normalize(x, center = FALSE, byrow = TRUE),
    structure(
      c(1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9, 12),
      dim = 4:3,
      scale = c(1, 1, 1, 1)
    )
  )
  expect_identical(
    normalize(x, scale = FALSE, byrow = TRUE),
    structure(
      c(-1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1),
      dim = 4:3,
      center = c(2, 5, 8, 11)
    )
  )
  expect_identical(
    normalize(x, center = FALSE, scale = FALSE, byrow = TRUE),
    structure(
      c(1, 4, 7, 10, 2, 5, 8, 11, 3, 6, 9, 12),
      dim = 4:3
    )
  )
})

test_that("can normalize data.frame", {

})

test_that("can normalize list", {

})
