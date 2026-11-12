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
      center = c(a = 2, b = NA, c = 8, d = 11),
      scale = c(a = 1, b = NA, c = 1, d = 1)
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
      center = c(a = 2, b = NA),
      scale = c(a = 1, b = NA)
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

test_that("can normalize time series", {
  x <- stats::ts(c(1, 3, 5, 7), start = c(2000, 2), frequency = 4)
  out <- normalize(x)
  expect_s3_class(out, "ts")
  expect_equal(
    stats::tsp(out),
    stats::tsp(x)
  )
  expect_equal(
    as.numeric(out),
    as.numeric(normalize(c(1, 3, 5, 7)))
  )
  expect_equal(
    mean(out),
    0
  )
  expect_equal(
    stats::sd(out),
    1
  )
  expect_equal(
    as.numeric(attr(out, "center")),
    4
  )
  expect_equal(
    as.numeric(attr(out, "scale")),
    stats::sd(c(1, 3, 5, 7))
  )
  expect_equal(
    as.numeric(normalize(x, scale = FALSE)),
    c(-3, -1, 1, 3)
  )
  x <- stats::ts(cbind(a = c(1, 2, 3), b = c(10, 30, 50)), start = 1990)
  out <- normalize(x)
  expect_s3_class(out, "mts")
  expect_equal(
    stats::tsp(out),
    stats::tsp(x)
  )
  expect_equal(
    colnames(out),
    c("a", "b")
  )
  expect_equal(
    unname(colMeans(out)),
    c(0, 0)
  )
  expect_equal(
    unname(apply(out, 2, stats::sd)),
    c(1, 1)
  )
})

test_that("preserves names and dimensions of vector", {
  expect_identical(
    normalize(c(a = 1, b = 2, c = 3)),
    structure(c(a = -1, b = 0, c = 1), center = 2, scale = 1)
  )
  expect_identical(
    normalize(c(a = 1L, b = 2L, c = 3L), scale = FALSE),
    structure(c(a = -1, b = 0, c = 1), center = 2)
  )
  x <- array(c(1, 2, 3), dim = 3, dimnames = list(c("a", "b", "c")))
  expect_identical(
    normalize(x),
    structure(
      c(-1, 0, 1),
      dim = 3L,
      dimnames = list(c("a", "b", "c")),
      center = 2,
      scale = 1
    )
  )
  expect_error(
    normalize(array(1:24, dim = 2:4)),
    "array with 3 dimensions"
  )
})

test_that("can normalize logical vector", {
  x <- c(TRUE, FALSE, TRUE)
  expect_equal(
    normalize(x),
    normalize(c(1, 0, 1))
  )
  expect_identical(
    normalize(c(a = TRUE, b = FALSE), scale = FALSE),
    structure(c(a = 0.5, b = -0.5), center = 0.5)
  )
  expect_warning(
    expect_identical(
      normalize(c(NA, NA)),
      structure(c(NA_real_, NA_real_), center = NaN, scale = NA_real_)
    ),
    "normalized result contains missing values"
  )
  expect_identical(
    normalize(list(1:3, x))[[2]],
    normalize(x)
  )
  expect_type(normalize(x), "double")
})

test_that("can normalize empty input", {
  expect_identical(
    normalize(numeric()),
    structure(numeric(), center = NaN, scale = NA_real_)
  )
  expect_identical(
    normalize(data.frame(a = numeric(), b = integer())),
    structure(
      data.frame(a = numeric(), b = numeric()),
      center = c(a = NaN, b = NaN),
      scale = c(a = NA_real_, b = NA_real_)
    )
  )
  expect_identical(
    normalize(matrix(numeric(), nrow = 0, ncol = 2)),
    structure(
      matrix(numeric(), nrow = 0, ncol = 2),
      center = c(NaN, NaN),
      scale = c(NA_real_, NA_real_)
    )
  )
  expect_identical(
    normalize(data.frame()),
    structure(data.frame(), center = numeric(), scale = numeric())
  )
  expect_identical(
    normalize(list()),
    list()
  )
})

test_that("preserves class and attributes of data.frame", {
  x <- structure(
    data.frame(
      a = c(1L, 2L, 4L), b = c(2, 4, 9), row.names = c("r1", "r2", "r3")
    ),
    class = c("special_frame", "data.frame"),
    note = "keep me"
  )
  attr(x$b, "label") <- "second column"
  x_text <- x
  x_text$c <- c("u", "v", "w")
  out_list <- list(
    normalize(x),
    normalize(x_text, ignore = 3),
    normalize(x, byrow = TRUE, ignore = 2)
  )
  for (out in out_list) {
    expect_s3_class(out, c("special_frame", "data.frame"), exact = TRUE)
    expect_identical(
      attr(out, "note"),
      "keep me"
    )
    expect_identical(
      attr(out$b, "label"),
      "second column"
    )
    expect_identical(
      row.names(out),
      c("r1", "r2", "r3")
    )
  }
  expect_equal(
    normalize(x)$a,
    (c(1, 2, 4) - 7 / 3) / stats::sd(c(1, 2, 4))
  )
  expect_equal(
    normalize(x_text, ignore = 3)$a,
    (c(1, 2, 4) - 7 / 3) / stats::sd(c(1, 2, 4))
  )
  expect_identical(
    normalize(x_text, ignore = 3)$c,
    c("u", "v", "w")
  )
  out <- normalize(x, byrow = TRUE, ignore = 2)
  expect_equal(
    out$a,
    c(-0.5, 2, -2.5) / c(sqrt(0.5), 1, sqrt(12.5))
  )
  expect_equal(
    as.numeric(out$b),
    c(0.5, 4, 2.5) / c(sqrt(0.5), 1, sqrt(12.5))
  )
  x <- data.frame(a = 1:3)
  x$m <- matrix(1:6, nrow = 3)
  expect_error(
    normalize(x),
    "matrices or data frames cannot be normalized: 2 \\(`m`\\)"
  )
  expect_identical(
    normalize(x, ignore = 2)$m,
    x$m
  )
})

test_that("can ignore non-numeric rows or columns", {
  x <- data.frame(a = 1:3, b = letters[1:3])
  expect_identical(
    normalize(x, byrow = TRUE, ignore = 1:3),
    structure(x, center = rep(NA_real_, 3), scale = rep(NA_real_, 3))
  )
  expect_identical(
    normalize(x, ignore = 1:2),
    structure(
      x,
      center = c(a = NA_real_, b = NA_real_),
      scale = c(a = NA_real_, b = NA_real_)
    )
  )
  expect_error(
    normalize(matrix(list(1, 2), nrow = 1), ignore = 1:2),
    "`x` must be a numeric or logical matrix"
  )
})

test_that("preserves row and column names in attributes", {
  x <- matrix(
    c(1, 2, 3, 4, 6, 9),
    nrow = 3,
    dimnames = list(c("r1", "r2", "r3"), c("a", "b"))
  )
  expect_equal(
    attr(normalize(x), "center"),
    c(a = 2, b = 19 / 3)
  )
  expect_equal(
    attr(normalize(x, ignore = 1), "center"),
    c(a = NA, b = 19 / 3)
  )
  expect_equal(
    attr(normalize(x, ignore = 1), "scale"),
    c(a = NA, b = stats::sd(c(4, 6, 9)))
  )
  expect_equal(
    attr(normalize(x, byrow = TRUE, ignore = 2), "center"),
    c(r1 = 2.5, r2 = NA, r3 = 6)
  )
  expect_equal(
    attr(normalize(as.data.frame(x), byrow = TRUE, ignore = 2), "scale"),
    c(r1 = stats::sd(c(1, 4)), r2 = NA, r3 = stats::sd(c(3, 9)))
  )
  expect_null(
    names(attr(normalize(unname(x), ignore = 1), "center"))
  )
})

test_that("warns about non-finite values", {
  expect_warning(
    expect_identical(
      normalize(c(2, 2, 2), center = FALSE),
      structure(c(Inf, Inf, Inf), scale = 0)
    ),
    "normalized result contains missing values or infinite values"
  )
  expect_warning(
    expect_identical(
      normalize(cbind(1, c(1, 2, 3))),
      structure(
        cbind(c(NaN, NaN, NaN), c(-1, 0, 1)),
        center = c(1, 2),
        scale = c(0, 1)
      )
    ),
    "a scale value is zero"
  )
  expect_warning(
    normalize(c(1, NA, 3), scale = FALSE),
    paste0(
      "^The normalized result contains missing values or infinite values\\. ",
      "This can happen when `x` has missing or infinite values\\.$"
    )
  )
  expect_silent(
    normalize(c(1, NA, 3), center = FALSE, scale = FALSE)
  )
  expect_silent(
    normalize(cbind(c(1, NA, 3), c(1, 2, 3)), ignore = 1)
  )
  expect_silent(
    normalize(data.frame(a = 1:3, b = c("x", NA, "z")), ignore = 2)
  )
  expect_silent(
    normalize(numeric())
  )
})

test_that("can normalize jointly with missing values", {
  x <- cbind(c(1, 5, NA, NA), c(2, 3, 4, 7))
  values <- c(1, 5, 2, 3, 4, 7)
  pooled_sd <- sqrt(
    (1 * stats::var(c(1, 5)) + 3 * stats::var(c(2, 3, 4, 7))) / 4
  )
  out <- suppressWarnings(normalize(x, jointly = list(1:2)))
  expect_equal(
    attr(out, "center"),
    rep(mean(values), 2)
  )
  expect_equal(
    attr(out, "scale"),
    rep(pooled_sd, 2)
  )
  expect_equal(
    as.numeric(out),
    as.numeric((x - mean(values)) / pooled_sd)
  )
  centered <- suppressWarnings(
    normalize(x, scale = FALSE, jointly = list(1:2))
  )
  expect_equal(
    mean(centered, na.rm = TRUE),
    0
  )
  by_row <- suppressWarnings(
    normalize(t(x), byrow = TRUE, jointly = list(1:2))
  )
  expect_identical(
    by_row,
    structure(
      t(unclass(out)),
      center = attr(out, "center"),
      scale = attr(out, "scale")
    )
  )
  x <- cbind(c(1, NA, NA), c(2, 3, 4), c(NA_real_, NA, NA))
  out <- suppressWarnings(normalize(x, jointly = list(1:3)))
  expect_equal(
    attr(out, "center"),
    rep(2.5, 3)
  )
  expect_equal(
    attr(out, "scale"),
    rep(1, 3)
  )
  expect_equal(
    out[, 1:2],
    cbind(c(-1.5, NA, NA), c(-0.5, 0.5, 1.5))
  )
  out <- suppressWarnings(normalize(x, jointly = list(c(1, 3))))
  expect_identical(
    attr(out, "scale"),
    c(NA, 1, NA)
  )
})

test_that("can center and scale in two steps", {
  x <- c(2, 4, 9)
  expect_equal(
    normalize(normalize(x, scale = FALSE), center = FALSE),
    normalize(x)
  )
})

test_that("preserves attributes of list", {
  x <- structure(list(u = 1:3, v = c(2, 4, 9)), note = "keep me")
  out <- normalize(x)
  expect_identical(
    attr(out, "note"),
    "keep me"
  )
  expect_identical(
    names(out),
    c("u", "v")
  )
  expect_identical(
    out$u,
    normalize(1:3)
  )
})

test_that("validates list elements", {
  expect_error(
    normalize(list(1:3, "a")),
    "Element 2 of the list cannot be normalized: `x` must be numeric"
  )
  expect_error(
    normalize(list(1:3, matrix(1:4, nrow = 2)), ignore = 3),
    "Element 2 of the list cannot be normalized: `ignore` contains 3"
  )
})

test_that("preserves attributes of time series", {
  x <- structure(stats::ts(c(1, 2, 4), start = 2001), note = "keep me")
  out <- normalize(x)
  expect_s3_class(out, "ts")
  expect_identical(
    attr(out, "note"),
    "keep me"
  )
  expect_identical(
    stats::tsp(out),
    stats::tsp(x)
  )
  expect_identical(
    normalize(stats::ts(1:5), center = FALSE, scale = FALSE),
    stats::ts(1:5)
  )
  x <- stats::ts(cbind(a = c(1, 2, 3), b = c(10, 30, 50)), start = 1990)
  out <- normalize(x, ignore = 1)
  expect_s3_class(out, "mts")
  expect_identical(
    attr(out, "center"),
    c(a = NA, b = 30)
  )
  expect_equal(
    as.numeric(out[, "a"]),
    c(1, 2, 3)
  )
  expect_equal(
    as.numeric(out[, "b"]),
    c(-1, 0, 1)
  )
})
