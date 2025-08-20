#' Fallback operator
#'
#' Returns `lhs` if not `NULL`, otherwise `rhs`.
#'
#' @name or_or
#' @aliases %||%
#' @usage lhs %||% rhs
#' @keywords internal
NULL

#' @rdname or_or
#' @export
`%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs

