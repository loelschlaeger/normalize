test_that("can normalize single numeric", {
  x <- 1
  expect_warning(
    expect_identical(
      normalize(x),
      structure(NA_real_, center = 1, scale = NA_real_)
    ),
    "'x' has NAs after normalization"
  )
  expect_warning(
    expect_identical(
      normalize(x, center = FALSE),
      structure(NA_real_, scale = NA_real_)
    ),
    "'x' has NAs after normalization"
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
      list(a = c(-2.5, -1.5, -0.5), b = c(0.5, 1.5, 2.5), c = c(-2.5, -1.5, -0.5), d = c(0.5, 1.5, 2.5)),
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
      list(a = c(-1, 0, 1), b = c("A", "B", "C"), c = c(-1, 0, 1), d = c(-1, 0, 1)),
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
        list(a = c(-1, 0, 1), b = c(-1, 0, 1), c = c(-1, 0, 1), d = c(-1, 0, 1)),
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
    "cannot work with objects of type character"
  )
  expect_error(
    normalize(matrix(LETTERS, nrow = 2)),
    "cannot work with objects of type character"
  )
  expect_error(
    normalize(diag),
    "no 'normalize' method for class function"
  )
})

test_that("allows and preserves attributes", {
  x <- structure(1:3, "test_attribute" = "test")
  expect_identical(
    normalize(x, scale = FALSE),
    structure(c(-1, 0, 1), center = 2, "test_attribute" = "test")
  )
})

