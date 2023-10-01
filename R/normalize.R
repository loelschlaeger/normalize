#' Centering and scaling of numeric data
#'
#' @description
#' normalize numeric data saved as a \code{vector}, \code{matrix},
#' \code{data.frame}, or \code{list} to zero mean and / or unit variance
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
#' used (if any) added as attributes \code{"center"} and \code{"scale"}
#'
#' @export

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

normalize.numeric <- function(
    x, center = TRUE, scale = TRUE, ...
  ) {
  if (center) {
    a <- mean(x)
    x <- structure(x - a, "center" = a)
  }
  if (scale) {
    b <- stats::sd(x)
    x <- structure(x / b, "scale" = b)
  }
  return(x)
}

#' @export
#' @rdname normalize

normalize.matrix <- function(
    x, center = TRUE, scale = TRUE, byrow = FALSE, ignore = integer(),
    jointly = integer(), ...
  ) {
  margin <- ifelse(byrow, 1, 2)
  apply(x, margin, normalize, center, scale)
}

#' @export
#' @rdname normalize

normalize.data.frame <- function(
    x, center = TRUE, scale = TRUE, byrow = FALSE, ignore = integer(),
    jointly = integer(), ...
  ) {
  margin <- ifelse(byrow, 1, 2)
  apply(x, margin, normalize, center, scale)
}

#' @export
#' @rdname normalize

normalize.list <- function(x, center = TRUE, scale = TRUE, ...) {
  lapply(x, normalize, ...)
}
