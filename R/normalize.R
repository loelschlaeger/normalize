#' Normalize numeric data
#'
#' @description
#' Center and scale vectors, matrices, data frames, time series, or lists of
#' these objects, i.e., subtract the mean and divide by the standard deviation.
#'
#' @details
#' A vector is normalized as a whole, while a matrix or data frame is
#' normalized column-wise (or row-wise if `byrow = TRUE`). A univariate time
#' series is treated like a vector and a multivariate time series like a
#' matrix. For a list, each element is normalized separately.
#'
#' Note that logical values are treated as `0` and `1`, and that normalized
#' values are of type `double`.
#'
#' # Missing values
#' Missing values are ignored when computing the mean and the standard
#' deviation, and they stay missing in the normalized object. The standard
#' deviation is missing if fewer than two values are available, and it is zero
#' if all values are equal. In these cases, scaling yields values that are not
#' finite. A warning is issued if the normalized values contain missing or
#' infinite values.
#'
#' # Joint normalization
#' Rows or columns that are normalized jointly (see the `jointly` argument)
#' share one centering and one scaling value. More precisely, the centering
#' value is the mean of all values in the group, and the scaling value is the
#' pooled standard deviation \deqn{\sqrt{\sum_k w_k s_k^2 / \sum_k w_k},}
#' where \eqn{s_k^2} denotes the variance of row or column \eqn{k} in the
#' group and the weight \eqn{w_k} is its number of non-missing values minus
#' one. Without missing values, this is the square root of the mean of the
#' variances. Note that the pooled standard deviation measures the variation
#' within the rows or columns, such that differences between their means do
#' not enter.
#'
#' @param x
#' An object to be normalized.
#'
#' @param center \[`logical(1)`\]\cr
#' Should values be centered to zero mean?
#'
#' @param scale \[`logical(1)`\]\cr
#' Should values be scaled to unit variance?
#'
#' @param byrow \[`logical(1)`\]\cr
#' Only relevant if `x` has two dimensions (rows and columns).
#'
#' In this case, set to `TRUE` to normalize row-wise or `FALSE` to normalize
#' column-wise (default).
#'
#' @param ignore \[`integer()`\]\cr
#' Only relevant if `x` has two dimensions (rows and columns).
#'
#' Indices to leave unchanged. With `byrow = TRUE`, these are row indices.
#' With `byrow = FALSE`, these are column indices. This is useful, for example,
#' to keep identifier or non-numeric columns in a data frame.
#'
#' @param jointly \[`list()`\]\cr
#' Only relevant if `x` has two dimensions (rows and columns).
#'
#' A list of disjoint index groups to normalize jointly, see details. With
#' `byrow = TRUE`, these are row indices. With `byrow = FALSE`, these are
#' column indices.
#'
#' @param ...
#' Further arguments to be passed to or from other methods.
#'
#' @return
#' A normalized version of `x`, preserving its class, dimensions, names, and
#' other attributes. Lists return a list with each element normalized.
#'
#' The centering and scaling values used are stored in the `"center"` and
#' `"scale"` attributes (only if the corresponding operation was requested).
#' For a vector, each attribute is a single number, while for a matrix or data
#' frame, each attribute has one element per column (or per row if
#' `byrow = TRUE`). The elements are named if the columns or rows have names,
#' and they are `NA` for ignored columns or rows. For a list, the elements of
#' the result have these attributes.
#'
#' @export
#'
#' @examples
#' # can normalize vectors, matrices, data.frames, and lists of such types
#' normalize(
#'   list(
#'     c(-3, 0, 3),
#'     matrix(1:12, nrow = 3, ncol = 4),
#'     data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
#'   )
#' )
#'
#' # centering and scaling values are stored as attributes
#' x <- normalize(c(2, 4, 9))
#' attr(x, "center")
#' attr(x, "scale")
#'
#' # can ignore columns (or rows)
#' normalize(
#'   data.frame(a = 1:3, b = c("A", "B", "C"), c = 7:9, d = 10:12),
#'   ignore = 2
#' )
#'
#' # can normalize columns (or rows) jointly
#' normalize(
#'   matrix(1:12, nrow = 3, ncol = 4),
#'   jointly = list(1:2, 3:4)
#' )
#'
#' # can normalize time series
#' normalize(stats::ts(c(1, 3, 5, 7), start = c(2000, 1), frequency = 4))

normalize <- function(x, center = TRUE, scale = TRUE, ...) {
  center <- .validate_logical(center, "center")
  scale <- .validate_logical(scale, "scale")
  if (is.character(x)) {
    stop(
      paste0(
        "`x` must be numeric, not character. ",
        "Use a numeric vector, matrix, data frame, or list; ",
        "for data frames, put non-numeric columns in `ignore`."
      ),
      call. = FALSE
    )
  }
  UseMethod("normalize")
}

#' @export
#' @rdname normalize

normalize.default <- function(x, ...) {
  stop(
    sprintf(
      "No `normalize()` method is available for objects with class %s.",
      .format_classes(class(x))
    ),
    call. = FALSE
  )
}

#' @export
#' @rdname normalize

normalize.ts <- function(x, center = TRUE, scale = TRUE, ...) {
  time_properties <- stats::tsp(x)
  values <- unclass(x)
  attr(values, "tsp") <- NULL
  out <- normalize(values, center = center, scale = scale, ...)
  attr(out, "tsp") <- time_properties
  class(out) <- class(x)
  out
}

#' @export
#' @rdname normalize

normalize.numeric <- function(x, center = TRUE, scale = TRUE, ...) {
  center <- .validate_logical(center, "center")
  scale <- .validate_logical(scale, "scale")
  if (!is.atomic(x)) {
    stop(
      "`x` must be an atomic numeric vector.",
      call. = FALSE
    )
  }
  if (length(dim(x)) > 1L) {
    stop(
      sprintf(
        paste0(
          "`x` must be one-dimensional or a matrix, ",
          "but it is an array with %s dimensions."
        ),
        length(dim(x))
      ),
      call. = FALSE
    )
  }
  if (!center && !scale) {
    return(x)
  }
  normalized <- .normalize_columns(
    matrix(x, ncol = 1), center = center, scale = scale
  )
  x[] <- normalized$values
  if (center) {
    attr(x, "center") <- normalized$center
  }
  if (scale) {
    attr(x, "scale") <- normalized$scale
  }
  x
}

#' @export
#' @rdname normalize

normalize.logical <- function(x, center = TRUE, scale = TRUE, ...) {
  normalize.numeric(x, center = center, scale = scale, ...)
}

#' @export
#' @rdname normalize

normalize.matrix <- function(
    x, center = TRUE, scale = TRUE, byrow = FALSE, ignore = integer(),
    jointly = list(), ...
  ) {
  center <- .validate_logical(center, "center")
  scale <- .validate_logical(scale, "scale")
  byrow <- .validate_logical(byrow, "byrow")
  if (!is.matrix(x) && !is.data.frame(x)) {
    stop(
      "`x` must be a matrix or data frame.",
      call. = FALSE
    )
  }
  n_units <- if (byrow) nrow(x) else ncol(x)
  unit <- if (byrow) "row" else "column"
  ignore <- .validate_indices(ignore, "ignore", n_units, unit)
  jointly <- .validate_jointly(jointly, n_units, unit, ignore)
  if (!center && !scale) {
    return(x)
  }
  units_to_normalize <- setdiff(seq_len(n_units), ignore)
  centering <- rep(NA_real_, n_units)
  scaling <- rep(NA_real_, n_units)
  if (length(units_to_normalize) > 0 || is.matrix(x)) {
    .validate_normalizable_data(x, byrow = byrow, ignore = ignore)
    values <- if (byrow) {
      t(as.matrix(x[units_to_normalize, , drop = FALSE]))
    } else {
      as.matrix(x[, units_to_normalize, drop = FALSE])
    }
    normalized <- .normalize_columns(
      values, center = center, scale = scale,
      jointly = lapply(jointly, match, table = units_to_normalize)
    )
    if (is.matrix(x)) {
      if (byrow) {
        x[units_to_normalize, ] <- t(normalized$values)
      } else {
        x[, units_to_normalize] <- normalized$values
      }
    } else {
      if (byrow) {
        for (column in seq_along(x)) {
          x[[column]][units_to_normalize] <- normalized$values[column, ]
        }
      } else {
        for (i in seq_along(units_to_normalize)) {
          x[[units_to_normalize[i]]][] <- normalized$values[, i]
        }
      }
    }
    if (center) {
      centering[units_to_normalize] <- normalized$center
    }
    if (scale) {
      scaling[units_to_normalize] <- normalized$scale
    }
  }
  unit_names <- if (byrow) rownames(x) else colnames(x)
  if (byrow && is.data.frame(x) && .row_names_info(x) < 0) {
    unit_names <- NULL
  }
  if (length(unit_names) > 0) {
    names(centering) <- unit_names
    names(scaling) <- unit_names
  }
  if (center) {
    attr(x, "center") <- centering
  }
  if (scale) {
    attr(x, "scale") <- scaling
  }
  return(x)
}

#' @export
#' @rdname normalize

normalize.data.frame <- function(
    x, center = TRUE, scale = TRUE, byrow = FALSE, ignore = integer(),
    jointly = list(), ...
  ) {
  if (!is.data.frame(x)) {
    stop(
      "`x` must be a data frame.",
      call. = FALSE
    )
  }
  normalize.matrix(
    x, center = center, scale = scale, byrow = byrow, ignore = ignore,
    jointly = jointly, ...
  )
}

#' @export
#' @rdname normalize

normalize.list <- function(x, center = TRUE, scale = TRUE, ...) {
  center <- .validate_logical(center, "center")
  scale <- .validate_logical(scale, "scale")
  if (!is.list(x)) {
    stop(
      "`x` must be a list.",
      call. = FALSE
    )
  }
  x[] <- lapply(seq_along(x), function(i) {
    tryCatch(
      normalize(x[[i]], center = center, scale = scale, ...),
      error = function(e) {
        stop(
          sprintf(
            "Element %s of the list cannot be normalized: %s",
            i, conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )
  })
  x
}

#' @keywords internal
#' @noRd

.normalize_columns <- function(x, center, scale, jointly = list()) {
  centering <- NULL
  scaling <- NULL
  if (center) {
    centering <- vapply(
      seq_len(ncol(x)),
      function(column) mean(x[, column], na.rm = TRUE),
      numeric(1)
    )
    n <- colSums(!is.na(x))
    for (join in jointly) {
      observed <- join[!is.na(centering[join])]
      centering[join] <- if (length(unique(n[observed])) > 1) {
        sum(centering[observed] * n[observed]) / sum(n[observed])
      } else {
        mean(centering[observed])
      }
    }
    x <- sweep(x, 2, centering, "-")
  }
  if (scale) {
    scaling <- vapply(
      seq_len(ncol(x)),
      function(column) stats::sd(x[, column], na.rm = TRUE),
      numeric(1)
    )
    n <- colSums(!is.na(x))
    for (join in jointly) {
      estimable <- join[n[join] > 1]
      numerator <- sum(scaling[estimable]^2 * (n[estimable] - 1))
      denominator <- sum(n[estimable] - 1)
      scaling[join] <- if (length(estimable) > 0) {
        sqrt(numerator / denominator)
      } else {
        NA_real_
      }
    }
    x <- sweep(x, 2, scaling, "/")
  }
  if (!all(is.finite(x))) {
    reasons <- "This can happen when `x` has missing or infinite values."
    if (scale) {
      reasons <- paste0(
        "This can happen when `x` has missing or infinite values, when fewer ",
        "than two non-missing values are available for a scale value, or when ",
        "a scale value is zero. Inspect `attr(result, \"scale\")` or use ",
        "`scale = FALSE`."
      )
    }
    warning(
      paste0(
        "The normalized result contains missing values or infinite values. ",
        reasons
      ),
      call. = FALSE
    )
  }
  list("values" = x, "center" = centering, "scale" = scaling)
}

.validate_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(
      sprintf("`%s` must be either TRUE or FALSE.", name),
      call. = FALSE
    )
  }
  x
}

.validate_indices <- function(x, name, n, unit) {
  if (!is.atomic(x) || !is.numeric(x)) {
    stop(
      sprintf(
        "`%s` must be a positive integer vector; use positions of %ss.",
        name, unit
      ),
      call. = FALSE
    )
  }
  if (length(x) == 0L) {
    return(integer())
  }
  if (anyNA(x)) {
    stop(
      sprintf("`%s` cannot contain missing values.", name),
      call. = FALSE
    )
  }
  if (any(!is.finite(x))) {
    stop(
      sprintf("`%s` cannot contain infinite values.", name),
      call. = FALSE
    )
  }
  if (any(x != floor(x))) {
    stop(
      sprintf(
        paste0(
          "`%s` must contain whole-number positions; ",
          "fractional values like %s are ambiguous."
        ),
        name, .format_values(x[x != floor(x)])
      ),
      call. = FALSE
    )
  }
  if (any(x < 1)) {
    stop(
      sprintf("`%s` must contain positive positions starting at 1.", name),
      call. = FALSE
    )
  }
  if (any(x > .Machine$integer.max)) {
    stop(
      sprintf("`%s` contains positions that are too large.", name),
      call. = FALSE
    )
  }
  index <- as.integer(x)
  if (anyDuplicated(index)) {
    stop(
      sprintf("`%s` cannot contain duplicate positions.", name),
      call. = FALSE
    )
  }
  if (any(index > n)) {
    stop(
      sprintf(
        "`%s` contains %s outside the available %ss. %s",
        name, .format_values(index[index > n]), unit, .format_range(n)
      ),
      call. = FALSE
    )
  }
  index
}

.validate_jointly <- function(jointly, n, unit, ignore) {
  if (!is.list(jointly)) {
    stop(
      paste0(
        "`jointly` must be a list of positive integer vectors, ",
        "e.g. `list(1:2)`."
      ),
      call. = FALSE
    )
  }
  if (length(jointly) == 0L) {
    return(list())
  }
  jointly <- lapply(seq_along(jointly), function(i) {
    index <- .validate_indices(
      jointly[[i]], sprintf("jointly[[%s]]", i), n, unit
    )
    if (length(index) == 0L) {
      stop(
        sprintf("`jointly[[%s]]` must contain at least one position.", i),
        call. = FALSE
      )
    }
    index
  })
  all_joint <- unlist(jointly, use.names = FALSE)
  if (anyDuplicated(all_joint)) {
    stop(
      "`jointly` groups must be disjoint; each position can appear only once.",
      call. = FALSE
    )
  }
  overlap <- intersect(all_joint, ignore)
  if (length(overlap) > 0L) {
    stop(
      sprintf(
        paste0(
          "`jointly` cannot include ignored %ss. ",
          "Remove %s from either `jointly` or `ignore`."
        ),
        unit, .format_values(overlap)
      ),
      call. = FALSE
    )
  }
  jointly
}

.validate_normalizable_data <- function(x, byrow, ignore) {
  if (is.data.frame(x)) {
    columns_to_normalize <- if (byrow) {
      seq_len(ncol(x))
    } else {
      setdiff(seq_len(ncol(x)), ignore)
    }
    invalid <- columns_to_normalize[
      !vapply(x[columns_to_normalize], .is_numeric_like, logical(1))
    ]
    if (length(invalid) > 0L && byrow) {
      stop(
        paste0(
          "Row-wise normalization of a data frame requires all columns ",
          "to be numeric or logical. Non-numeric columns: ",
          .format_columns(x, invalid), ". Select numeric columns before ",
          "calling `normalize()`, or use column-wise normalization with ",
          "`byrow = FALSE` and list non-numeric columns in `ignore`."
        ),
        call. = FALSE
      )
    }
    if (length(invalid) > 0L) {
      stop(
        paste0(
          "Columns selected for normalization must be numeric or logical. ",
          "Non-numeric columns: ", .format_columns(x, invalid), ". ",
          "Add their positions to `ignore` or select numeric columns before ",
          "calling `normalize()`."
        ),
        call. = FALSE
      )
    }
    nested <- columns_to_normalize[
      !vapply(
        x[columns_to_normalize], function(col) is.null(dim(col)), logical(1)
      )
    ]
    if (length(nested) > 0L) {
      stop(
        paste0(
          "Columns of a data frame that are matrices or data frames cannot ",
          "be normalized: ", .format_columns(x, nested), ". Remove them ",
          "before calling `normalize()`, or use column-wise normalization ",
          "with `byrow = FALSE` and add their positions to `ignore`."
        ),
        call. = FALSE
      )
    }
    return(invisible(TRUE))
  }
  if (!is.numeric(x) && !is.logical(x)) {
    stop(
      paste0(
        "`x` must be a numeric or logical matrix. ",
        "Character and factor data must be converted or removed ",
        "before normalization."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.is_numeric_like <- function(x) {
  is.numeric(x) || is.logical(x)
}

.format_classes <- function(x) {
  paste(sprintf("`%s`", x), collapse = ", ")
}

.format_values <- function(x, n = 5L) {
  x <- unique(x)
  out <- paste(x[seq_len(min(length(x), n))], collapse = ", ")
  if (length(x) > n) {
    out <- paste0(out, ", ...")
  }
  out
}

.format_range <- function(n) {
  if (n == 0L) {
    "There are no available positions."
  } else {
    sprintf("Use values between 1 and %s.", n)
  }
}

.format_columns <- function(x, index) {
  names <- names(x)[index]
  labels <- ifelse(
    is.na(names) | names == "",
    as.character(index),
    sprintf("%s (`%s`)", index, names)
  )
  paste(labels, collapse = ", ")
}
