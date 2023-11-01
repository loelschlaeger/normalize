#' Centering and scaling of numeric data
#'
#' @description
#' normalize \code{numeric} data saved as a \code{vector}, \code{matrix},
#' \code{data.frame}, or \code{list} to have zero mean and / or unit variance
#'
#' @param x
#' an object to be normalized
#'
#' @param center
#' \code{TRUE} to normalize to zero mean or \code{FALSE} for no centering
#'
#' @param scale
#' \code{TRUE} to normalize to unit variance or \code{FALSE} for no scaling
#'
#' @param byrow
#' \code{TRUE} to normalize row-wise or \code{FALSE} to normalize column-wise
#'
#' @param ignore
#' an \code{integer} vector of column indices (or row indices if
#' \code{byrow = TRUE}) to not normalize
#'
#' @param jointly
#' a \code{list} of disjoint \code{integer} vectors of column indices (or row
#' indices if \code{byrow = TRUE}) to normalize jointly
#'
#' @param ...
#' further arguments to be passed to or from other methods
#'
#' @return
#' the normalized input \code{x} with the \code{numeric} centering and scaling
#' values used (if any) added as attributes \code{"center"} and \code{"scale"}
#' (ignored columns and rows get centering and scaling values of \code{NA})
#'
#' @export
#'
#' @examples
#' # can normalize numeric vectors, matrices, data.frames, and lists of those
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
  if (is.character(x)) {
    stop("sorry, I cannot work with objects of type 'character'")
  }
  stopifnot(
    "please set 'center' to TRUE or FALSE" = isTRUE(center) || isFALSE(center),
    "please set 'scale' to TRUE or FALSE" = isTRUE(scale) || isFALSE(scale)
  )
  UseMethod("normalize")
}

#' @export
#' @rdname normalize

normalize.numeric <- function(x, center = TRUE, scale = TRUE, ...) {
  structure(
    normalize(as.matrix(x), center = center, scale = scale, byrow = FALSE),
    "dim" = NULL
  )
}

#' @export
#' @rdname normalize

normalize.matrix <- function(
    x, center = TRUE, scale = TRUE, byrow = FALSE, ignore = integer(),
    jointly = list(), ...
  ) {
  stopifnot(
    "please set 'byrow' to TRUE or FALSE" = isTRUE(byrow) || isFALSE(byrow),
    "'ignore' should be an index vector" = is.vector(ignore) && is.numeric(ignore),
    "'jointly' should be a list" = is.list(jointly)
  )
  if (center) {
    centering <- center_values(x, byrow = byrow, ignore = ignore, jointly = jointly)
    if (length(ignore) > 0) {
      if (byrow) {
        x[-ignore, ] <- sweep(x[-ignore, , drop = FALSE], 1, centering[-ignore], "-")
      } else {
        x[, -ignore] <- sweep(x[, -ignore, drop = FALSE], 2, centering[-ignore], "-")
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
    scaling <- scale_values(x, byrow = byrow, ignore = ignore, jointly = jointly)
    if (length(ignore) > 0) {
      if (byrow) {
        x[-ignore, ] <- sweep(x[-ignore, , drop = FALSE], 1, scaling[-ignore], "/")
      } else {
        x[, -ignore] <- sweep(x[, -ignore, drop = FALSE], 2, scaling[-ignore], "/")
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
    warning("'x' has NAs after normalization")
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
  normalize.matrix(
    x, center = center, scale = scale, byrow = byrow, ignore = ignore,
    jointly = jointly, ...
  )
}

#' @export
#' @rdname normalize

normalize.list <- function(x, center = TRUE, scale = TRUE, ...) {
  lapply(x, normalize, center = center, scale = scale, ...)
}

#' @keywords internal

center_values <- function(
    x, byrow = TRUE, ignore = integer(), jointly = list()
  ) {
  centering <- rep(NA_real_, ifelse(byrow, nrow(x), ncol(x)))
  indices <- if (byrow) seq_len(nrow(x)) else seq_len(ncol(x))
  ignore <- as.integer(ignore)
  jointly <- lapply(jointly, as.integer)
  stopifnot(
    "indices in 'ignore' are out of bound" = ignore %in% indices,
    "indices in 'ignore' are not unique" = length(ignore) == length(unique(ignore))
  )
  stopifnot(
    "indices in 'jointly' are out of bound" = all(sapply(jointly, `%in%`, indices)),
    "indices in 'jointly' are not unique" = length(unlist(jointly)) == length(unique(unlist(jointly)))
  )
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

#' @keywords internal

scale_values <- function(
    x, byrow = TRUE, ignore = integer(), jointly = list()
  ) {
  scaling <- rep(NA_real_, ifelse(byrow, nrow(x), ncol(x)))
  indices <- if (byrow) seq_len(nrow(x)) else seq_len(ncol(x))
  ignore <- as.integer(ignore)
  jointly <- lapply(jointly, as.integer)
  stopifnot(
    "indices in 'ignore' are out of bound" = ignore %in% indices,
    "indices in 'ignore' are not unique" = length(ignore) == length(unique(ignore))
  )
  stopifnot(
    "indices in 'jointly' are out of bound" = all(sapply(jointly, `%in%`, indices)),
    "indices in 'jointly' are not unique" = length(unlist(jointly)) == length(unique(unlist(jointly)))
  )
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
    scaling[join] <- sqrt(sum((scaling[join]^2 * (n - 1))) / (length(scaling[join]) * (n - 1)))
  }
  return(scaling)
}

