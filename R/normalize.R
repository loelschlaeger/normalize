#' Normalize numeric data
#'
#' @description
#' Center and scale vectors, matrices, data frames, or lists of these objects.
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
#' A list of disjoint index groups to normalize jointly. With `byrow = TRUE`,
#' these are row indices. With `byrow = FALSE`, these are column indices.
#'
#' @param ...
#' Further arguments to be passed to or from other methods.
#'
#' @return
#' A normalized version of `x`, preserving attributes where possible. Lists
#' return a list with each element normalized. The centering and scaling values
#' used are stored in the `"center"` and `"scale"` attributes where relevant.
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

normalize.numeric <- function(x, center = TRUE, scale = TRUE, ...) {
  center <- .validate_logical(center, "center")
  scale <- .validate_logical(scale, "scale")
  if (!is.atomic(x)) {
    stop(
      "`x` must be an atomic numeric vector.",
      call. = FALSE
    )
  }
  if (!is.null(dim(x))) {
    stop(
      "`x` must be one-dimensional. Use `normalize()` on a matrix directly.",
      call. = FALSE
    )
  }
  dim(x) <- c(length(x), 1)
  structure(
    normalize.matrix(x, center = center, scale = scale, byrow = FALSE),
    "dim" = NULL
  )
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
  if (center || scale) {
    .validate_normalizable_data(x, byrow = byrow, ignore = ignore)
  }
  if (center) {
    centering <- .center_values(
      x, byrow = byrow, ignore = ignore, jointly = jointly
    )
    if (length(ignore) > 0) {
      if (byrow) {
        x <- .extract_keep_attr(
          x,
          rows = -ignore,
          value = sweep(
            x[-ignore, , drop = FALSE], 1, centering[-ignore], "-"
          )
        )
      } else {
        x <- .extract_keep_attr(
          x,
          columns = -ignore,
          value = sweep(
            x[, -ignore, drop = FALSE], 2, centering[-ignore], "-"
          )
        )
      }
    } else {
      if (byrow) {
        x <- sweep(x, 1, centering, "-")
      } else {
        x <- sweep(x, 2, centering, "-")
      }
    }
  }
  if (scale) {
    scaling <- .scale_values(
      x, byrow = byrow, ignore = ignore, jointly = jointly
    )
    if (length(ignore) > 0) {
      if (byrow) {
        x <- .extract_keep_attr(
          x,
          rows = -ignore,
          value = sweep(
            x[-ignore, , drop = FALSE], 1, scaling[-ignore], "/"
          )
        )
      } else {
        x <- .extract_keep_attr(
          x,
          columns = -ignore,
          value = sweep(
            x[, -ignore, drop = FALSE], 2, scaling[-ignore], "/"
          )
        )
      }
    } else {
      if (byrow) {
        x <- sweep(x, 1, scaling, "/")
      } else {
        x <- sweep(x, 2, scaling, "/")
      }
    }
  }
  if (anyNA(x)) {
    warning(
      paste0(
        "The normalized result contains missing values. ",
        "This can happen when `x` has missing values, a unit has fewer ",
        "than two non-missing values, or a scale value is zero; inspect ",
        "`attr(result, \"scale\")` or use `scale = FALSE`."
      ),
      call. = FALSE
    )
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
  lapply(x, normalize, center = center, scale = scale, ...)
}

.center_values <- function(
    x, byrow = TRUE, ignore = integer(), jointly = list()
  ) {
  centering <- rep(NA_real_, ifelse(byrow, nrow(x), ncol(x)))
  if (length(ignore) > 0) {
    if (byrow) {
      x <- x[-ignore, , drop = FALSE]
    } else {
      x <- x[, -ignore, drop = FALSE]
    }
  }
  means <- apply(x, ifelse(byrow, 1, 2), mean, na.rm = TRUE, simplify = TRUE)
  if (length(ignore) > 0) {
    centering[-ignore] <- means
  } else {
    centering <- means
  }
  for (join in jointly) {
    centering[join] <- mean(centering[join], na.rm = TRUE)
  }
  return(centering)
}

.scale_values <- function(
    x, byrow = TRUE, ignore = integer(), jointly = list()
  ) {
  scaling <- rep(NA_real_, ifelse(byrow, nrow(x), ncol(x)))
  if (length(ignore) > 0) {
    if (byrow) {
      x <- x[-ignore, , drop = FALSE]
    } else {
      x <- x[, -ignore, drop = FALSE]
    }
  }
  sds <- apply(x, ifelse(byrow, 1, 2), stats::sd, na.rm = TRUE, simplify = TRUE)
  if (length(ignore) > 0) {
    scaling[-ignore] <- sds
  } else {
    scaling <- sds
  }
  n <- ifelse(byrow, ncol(x), nrow(x))
  for (join in jointly) {
    numerator <- sum(scaling[join]^2 * (n - 1))
    denominator <- length(scaling[join]) * (n - 1)
    scaling[join] <- sqrt(numerator / denominator)
  }
  return(scaling)
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
      sprintf(
        paste0(
          "`jointly` must be a list of positive integer vectors, ",
          "e.g. `list(1:2)`."
        )
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
    if (byrow) {
      rows_to_normalize <- setdiff(seq_len(nrow(x)), ignore)
      if (length(rows_to_normalize) == 0L) {
        return(invisible(TRUE))
      }
      invalid <- which(!vapply(x, .is_numeric_like, logical(1)))
      if (length(invalid) > 0L) {
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
      return(invisible(TRUE))
    }
    columns_to_normalize <- setdiff(seq_len(ncol(x)), ignore)
    invalid <- columns_to_normalize[
      !vapply(x[columns_to_normalize], .is_numeric_like, logical(1))
    ]
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
    return(invisible(TRUE))
  }
  units_to_normalize <- setdiff(
    seq_len(if (byrow) nrow(x) else ncol(x)),
    ignore
  )
  if (length(units_to_normalize) == 0L) {
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
  out <- paste(utils::head(x, n), collapse = ", ")
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

.extract_keep_attr <- function(x, ...) {
  args <- list(...)
  rows <- args$rows
  columns <- args$columns
  value <- args$value
  if (is.null(rows)) {
    rows <- seq_len(nrow(x))
  }
  if (is.null(columns)) {
    columns <- seq_len(ncol(x))
  }
  updated <- x
  updated[rows, columns] <- value
  attrs <- attributes(x)
  if (!is.null(attributes(updated)$dim)) {
    attrs$dim <- attributes(updated)$dim
  } else {
    attrs$dim <- NULL
  }
  for (a in c("names", "dimnames", "row.names")) {
    if (!is.null(attributes(updated)[[a]])) {
      attrs[[a]] <- attributes(updated)[[a]]
    } else {
      attrs[[a]] <- NULL
    }
  }
  mostattributes(updated) <- attrs
  return(updated)
}
