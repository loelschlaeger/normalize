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
#'
#' @export
#'
#' @examples
#' # can normalize vectors, matrices, data.frames, and lists of such types
#' normalize(
#'   list(
#'     1:10,
#'     matrix(1:12, nrow = 3, ncol = 4),
#'     data.frame(a = 1:6, b = -6:-1)
#'   )
#' )
#'
#' # can ignore columns (or rows)
#' # TODO
#'
#' # can normalize columns (or rows) jointly
#' # TODO

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
    jointly = list(), ...
  ) {
  stopifnot(
    "please set 'byrow' to TRUE or FALSE" = isTRUE(byrow) || isFALSE(byrow),
    "'jointly' should be a list" = is.list(jointly)
  )
  margin <- ifelse(byrow, 1, 2)
  indices <- if (byrow) seq_len(nrow(x)) else seq_len(ncol(x))
  ignore <- as.integer(ignore)
  stopifnot(
    "indices in 'ignore' are out of bound" = ignore %in% indices,
    "indices in 'ignore' are not unique" = length(ignore) == length(unique(ignore))
  )
  jointly <- lapply(jointly, as.integer)
  stopifnot(
    "indices in 'jointly' are out of bound" = all(sapply(jointly, `%in%`, indices)),
    "indices in 'jointly' are not unique" = length(unlist(jointly)) == length(unique(unlist(jointly)))
  )
  normalized <- apply(x, margin, normalize, center = center, scale = scale, simplify = FALSE)
  bring_in_shape(
    x, normalized, byrow = byrow, ignore = ignore, jointly = jointly
  )
}

#' @export
#' @rdname normalize

normalize.data.frame <- function(
    x, center = TRUE, scale = TRUE, byrow = FALSE, ignore = integer(),
    jointly = list(), ...
  ) {
  normalized <- normalize(
    as.matrix(x), center = center, scale = scale, byrow = byrow, ignore = ignore,
    jointly = jointly
  )
  bring_in_shape(
    x, normalized, byrow = byrow, ignore = ignore, jointly = jointly,
    attributes = attributes(x)
  )
}

#' @export
#' @rdname normalize

normalize.list <- function(x, center = TRUE, scale = TRUE, ...) {
  lapply(x, normalize, center = center, scale = scale, ...)
}

#' @keywords internal

bring_in_shape <- function(x, normalized, byrow, ignore, jointly, ...) {
  UseMethod("bring_in_shape")
}

#' @keywords internal

bring_in_shape.matrix <- function(x, normalized, byrow, ignore, jointly) {
  attributes <- sapply(normalized, attributes)
  attribute_names <- rownames(attributes)
  out <- matrix(NA_real_, nrow = nrow(x), ncol = ncol(x))
  for (i in 1:ncol(x)) {
    out[, i] <- normalized[[i]]
  }
  for (attribute_name in attribute_names) {
    attr(out, attribute_name) <- unlist(attributes[attribute_name, ])
  }
  return(out)
}

#' @keywords internal

bring_in_shape.data.frame <- function(
    x, normalized, byrow, ignore, jointly, attributes
  ) {
  return(normalized)
}



