# helper_standardize <- function(
#     argument, byrow = FALSE, center = TRUE, scale = TRUE, ignore = integer(),
#     jointly = list()
# ) {
#   vector_flag <- is.atomic(argument) && is.null(dim(argument))
#   if (vector_flag) {
#     argument <- as.data.frame(argument)
#     byrow <- FALSE
#     ignore <- integer()
#     jointly <- list()
#   } else if (is.data.frame(argument) || is.matrix(argument)) {
#     checkmate::assert_logical(byrow, len = 1)
#     indices <- if (byrow) {
#       seq_len(nrow(argument))
#     } else {
#       seq_len(ncol(argument))
#     }
#     checkmate::assert_integerish(ignore, lower = 1)
#     if (!all(ignore %in% indices)) {
#       cli::cli_abort(
#         "Argument {.var ignore} is out of bound.",
#         call = NULL
#       )
#     }
#     checkmate::assert_list(jointly)
#     if (length(jointly) > 0) {
#       for (joint in jointly) {
#         if (!checkmate::test_integerish(joint)) {
#           cli::cli_abort(
#             "Argument {.var jointly} must contain index vectors.",
#             call = NULL
#           )
#         }
#         if (!all(joint %in% indices)) {
#           cli::cli_abort(
#             "Argument {.var jointly} is out of bound.",
#             call = NULL
#           )
#         }
#       }
#       if (length(unlist(jointly)) != length(unique(unlist(jointly)))) {
#         cli::cli_abort(
#           "Elements in {.var jointly} must be exclusive.",
#           call = NULL
#         )
#       }
#       if (length(intersect(unlist(jointly), ignore)) > 0) {
#         cli::cli_abort(
#           "Cannot have same elements in {.var jointly} and {.var ignored}.",
#           call = NULL
#         )
#       }
#     }
#   } else {
#     cli::cli_abort(
#       "Argument cannot be standardized.",
#       call = NULL
#     )
#   }
#   margin <- ifelse(byrow, 1, 2)
#   center_values <- rep(0, dim(argument)[margin])
#   scale_values <- rep(1, dim(argument)[margin])
#   if (center) {
#     center_values <- apply(
#       argument, margin, mean, na.rm = TRUE, simplify = TRUE
#     )
#     center_values[ignore] <- 0
#     for (join in jointly) {
#       center_values[join] <- mean(center_values[join], na.rm = TRUE)
#     }
#     argument <- sweep(argument, margin, center_values, "-")
#   }
#   if (scale) {
#     scale_values <- apply(
#       argument, margin, stats::sd, na.rm = TRUE, simplify = TRUE
#     )
#     scale_values[ignore] <- 1
#     for (join in jointly) {
#       scale_values[join] <- if (byrow) {
#         stats::sd(as.matrix(argument[join, ]), na.rm = TRUE)
#       } else {
#         stats::sd(as.matrix(argument[, join]), na.rm = TRUE)
#       }
#     }
#     argument <- sweep(argument, margin, scale_values, "/")
#   }
#   if (anyNA(argument)) {
#     cli::cli_warn("Argument has NAs after standardization.")
#   }
#   if (vector_flag) {
#     argument <- argument[, 1, drop = TRUE]
#   }
#   if (center) {
#     attr(argument, "standardized:center") <- as.numeric(center_values)
#   }
#   if (scale) {
#     attr(argument, "standardized:scale") <- as.numeric(scale_values)
#   }
#   return(argument)
# }
