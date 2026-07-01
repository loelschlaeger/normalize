test_that("can normalize single numeric", {
  x <- 1
  expect_warning(
    expect_identical(
      normalize(x),
      structure(NA_real_, center = 1, scale = NA_real_)
    ),
    "normalized result contains missing values"
  )
  expect_warning(
    expect_identical(
      normalize(x, center = FALSE),
      structure(NA_real_, scale = NA_real_)
    ),
    "normalized result contains missing values"
  )
  expect_identical(
    normalize(x, scale = FALSE),
    structure(0, center = 1)
  )
  expect_identical(
    normalize(x, center = FALSE, scale = FALSE),
    x
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
    x
  )
  expect_identical(
    normalize(matrix(1:4, nrow = 2), ignore = 1:2),
    structure(
      matrix(as.numeric(1:4), nrow = 2),
      center = c(NA_real_, NA_real_),
      scale = c(NA_real_, NA_real_)
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
    x
  )
})

test_that("can ignore when normalizing", {
  x <- matrix(1:12, nrow = 3, ncol = 4)
  expect_identical(
    normalize(x, ignore = 1),
    structure(
      c(1, 2, 3, -1, 0, 1, -1, 0, 1, -1, 0, 1),
      dim = 3:4,
      center = c(NA, 5, 8, 11),
      scale = c(NA, 1, 1, 1)
    )
  )
  expect_identical(
    normalize(x, ignore = c(1, 2, 3)),
    structure(
      c(1, 2, 3, 4, 5, 6, 7, 8, 9, -1, 0, 1),
      dim = 3:4,
      center = c(NA, NA, NA, 11),
      scale = c(NA, NA, NA, 1)
    )
  )
  x <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
  expect_identical(
    normalize(x, byrow = TRUE, ignore = 1),
    structure(
      c(1, -1, -1, -1, 2, 0, 0, 0, 3, 1, 1, 1),
      dim = 4:3,
      center = c(NA, 5, 8, 11),
      scale = c(NA, 1, 1, 1)
    )
  )
  expect_identical(
    normalize(x, byrow = TRUE, ignore = c(1, 2, 3)),
    structure(
      c(1, 4, 7, -1, 2, 5, 8, 0, 3, 6, 9, 1),
      dim = 4:3,
      center = c(NA, NA, NA, 11),
      scale = c(NA, NA, NA, 1)
    )
  )
})

test_that("can normalize jointly", {
  x <- matrix(1:12, nrow = 3, ncol = 4)
  expect_identical(
    normalize(x, jointly = list(c(1, 2))),
    structure(
      c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5, -1, 0, 1, -1, 0, 1),
      dim = 3:4,
      center = c(3.5, 3.5, 8, 11),
      scale = c(1, 1, 1, 1)
    )
  )
  expect_identical(
    normalize(x, jointly = list(1:4)),
    structure(
      c(-5.5, -4.5, -3.5, -2.5, -1.5, -0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5),
      dim = 3:4,
      center = c(6.5, 6.5, 6.5, 6.5),
      scale = c(1, 1, 1, 1)
    )
  )
  x <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)
  expect_identical(
    normalize(x, byrow = TRUE, jointly = list(c(1, 2))),
    structure(
      c(-2.5, 0.5, -1, -1, -1.5, 1.5, 0, 0, -0.5, 2.5, 1, 1),
      dim = 4:3,
      center = c(3.5, 3.5, 8, 11),
      scale = c(1, 1, 1, 1)
    )
  )
  expect_identical(
    normalize(x, byrow = TRUE, jointly = list(1:4)),
    structure(
      c(-5.5, -2.5, 0.5, 3.5, -4.5, -1.5, 1.5, 4.5, -3.5, -0.5, 2.5, 5.5),
      dim = 4:3,
      center = c(6.5, 6.5, 6.5, 6.5),
      scale = c(1, 1, 1, 1)
    )
  )
})

test_that("can normalize data.frame", {
  x <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  expect_identical(
    normalize(x, center = FALSE, scale = FALSE),
    x
  )
  expect_identical(
    normalize(x),
    structure(
      list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1), d = c(-1, 0, 1)),
      class = "data.frame",
      row.names = c(NA, -3L),
      center = c(a = 2, b = 5, c = 8, d = 11),
      scale = c(a = 1, b = 1, c = 1, d = 1)
    )
  )
  expect_identical(
    normalize(t(x), byrow = TRUE),
    structure(
      c(-1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1),
      dim = 4:3,
      dimnames = list(c("a", "b", "c", "d"), NULL),
      center = c(a = 2, b = 5, c = 8, d = 11),
      scale = c(a = 1, b = 1, c = 1, d = 1)
    )
  )
  expect_identical(
    normalize(x, jointly = list(1:2, 3:4)),
    structure(
      list(
        a = c(-2.5, -1.5, -0.5),
        b = c(0.5, 1.5, 2.5),
        c = c(-2.5, -1.5, -0.5),
        d = c(0.5, 1.5, 2.5)
      ),
      class = "data.frame",
      row.names = c(NA, -3L),
      center = c(a = 3.5, b = 3.5, c = 9.5, d = 9.5),
      scale = c(a = 1, b = 1, c = 1, d = 1)
    )
  )
  x$b <- LETTERS[1:3]
  expect_identical(
    normalize(x, ignore = 2),
    structure(
      list(
        a = c(-1, 0, 1),
        b = c("A", "B", "C"),
        c = c(-1, 0, 1),
        d = c(-1, 0, 1)
      ),
      row.names = c(NA, -3L),
      class = "data.frame",
      center = c(2, NA, 8, 11),
      scale = c(1, NA, 1, 1)
    )
  )
})

test_that("can normalize list", {
  expect_equal(
    normalize(
      list(
        c(-3, 0, 3),
        matrix(1:12, nrow = 3, ncol = 4),
        data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
      )
    ),
    list(
      structure(
        c(-1, 0, 1),
        center = 0,
        scale = 3
      ),
      structure(
        c(-1, 0, 1, -1, 0, 1, -1, 0, 1, -1, 0, 1),
        dim = 3:4,
        center = c(2, 5, 8, 11),
        scale = c(1, 1, 1, 1)
      ),
      structure(
        list(
          a = c(-1, 0, 1),
          b = c(-1, 0, 1),
          c = c(-1, 0, 1),
          d = c(-1, 0, 1)
        ),
        class = "data.frame",
        row.names = c(NA, -3L),
        center = c(a = 2, b = 5, c = 8, d = 11),
        scale = c(a = 1, b = 1, c = 1, d = 1)
      )
    )
  )
})

test_that("cannot normalize everything", {
  expect_error(
    normalize(LETTERS),
    "`x` must be numeric"
  )
  expect_error(
    normalize(matrix(LETTERS, nrow = 2)),
    "`x` must be numeric"
  )
  expect_error(
    normalize(diag),
    "No `normalize\\(\\)` method is available"
  )
  expect_error(
    normalize:::normalize.numeric(
      structure(list(1), class = "numeric")
    ),
    "atomic numeric vector"
  )
  expect_error(
    normalize:::normalize.numeric(matrix(1:4, nrow = 2)),
    "one-dimensional"
  )
  expect_error(
    normalize:::normalize.matrix(1:3),
    "`x` must be a matrix or data frame"
  )
  expect_error(
    normalize:::normalize.data.frame(1:3),
    "`x` must be a data frame"
  )
  expect_error(
    normalize:::normalize.list(1:3),
    "`x` must be a list"
  )
})

test_that("validates scalar logical arguments", {
  expect_error(
    normalize(1:3, center = NA),
    "`center` must be either TRUE or FALSE"
  )
  expect_error(
    normalize(1:3, scale = c(TRUE, FALSE)),
    "`scale` must be either TRUE or FALSE"
  )
  expect_error(
    normalize(matrix(1:4, nrow = 2), byrow = NA),
    "`byrow` must be either TRUE or FALSE"
  )
})

test_that("validates ignore and jointly indices", {
  x <- matrix(1:4, nrow = 2)
  expect_error(
    normalize(x, ignore = NULL),
    "`ignore` must be a positive integer vector"
  )
  expect_error(
    normalize(x, jointly = NULL),
    "`jointly` must be a list"
  )
  expect_error(
    normalize(x, ignore = 1.5),
    "whole-number positions"
  )
  expect_error(
    normalize(x, ignore = NA_real_),
    "cannot contain missing values"
  )
  expect_error(
    normalize(x, ignore = list(1)),
    "positive integer vector"
  )
  expect_error(
    normalize(x, ignore = Inf),
    "cannot contain infinite values"
  )
  expect_error(
    normalize(x, ignore = 3),
    "outside the available columns"
  )
  expect_error(
    normalize(matrix(numeric(), nrow = 3), ignore = 1),
    "There are no available positions"
  )
  expect_error(
    normalize(x, ignore = seq(1.5, 6.5)),
    "\\.\\.\\."
  )
  expect_error(
    normalize(x, ignore = 0),
    "positive positions starting at 1"
  )
  expect_error(
    normalize(x, ignore = .Machine$integer.max + 1),
    "positions that are too large"
  )
  expect_error(
    normalize(x, ignore = c(1, 1)),
    "duplicate positions"
  )
  expect_error(
    normalize(x, jointly = 1:2),
    "`jointly` must be a list"
  )
  expect_error(
    normalize(x, jointly = list(integer())),
    "must contain at least one position"
  )
  expect_error(
    normalize(x, jointly = list(1:2, 2)),
    "groups must be disjoint"
  )
  expect_error(
    normalize(x, ignore = 1, jointly = list(1:2)),
    "cannot include ignored columns"
  )
})

test_that("validates non-numeric data selected for normalization", {
  x <- data.frame(a = 1:3, b = letters[1:3])
  expect_error(
    normalize(x),
    "Non-numeric columns: 2 \\(`b`\\)"
  )
  expect_identical(
    normalize(x, center = FALSE, scale = FALSE),
    x
  )
  expect_identical(
    normalize(x, ignore = 2),
    structure(
      list(a = c(-1, 0, 1), b = c("a", "b", "c")),
      class = "data.frame",
      row.names = c(NA, -3L),
      center = c(2, NA),
      scale = c(1, NA)
    )
  )
  expect_identical(
    normalize(data.frame(a = 1:3), byrow = TRUE, ignore = 1:3),
    structure(
      data.frame(a = 1:3),
      center = rep(NA_real_, 3),
      scale = rep(NA_real_, 3)
    )
  )
  expect_equal(
    normalize(data.frame(a = 1:2, b = 3:4), byrow = TRUE),
    structure(
      data.frame(
        a = c(-1 / sqrt(2), -1 / sqrt(2)),
        b = c(1 / sqrt(2), 1 / sqrt(2))
      ),
      center = c(2, 3),
      scale = c(sqrt(2), sqrt(2))
    )
  )
  expect_error(
    normalize(x, byrow = TRUE),
    "Row-wise normalization of a data frame requires all columns"
  )
  expect_error(
    normalize(data.frame(a = 1:3, b = factor(letters[1:3]))),
    "Non-numeric columns: 2 \\(`b`\\)"
  )
  expect_error(
    normalize(matrix(list(1, 2), nrow = 1)),
    "`x` must be a numeric or logical matrix"
  )
})

test_that("allows and preserves attributes", {
  x <- structure(1:3, "test_attribute" = "test")
  expect_identical(
    normalize(x, scale = FALSE),
    structure(c(-1, 0, 1), center = 2, "test_attribute" = "test")
  )
})
