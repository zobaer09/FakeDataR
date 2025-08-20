# Internal helper(s) — not exported
#' @keywords internal
#' @noRd

# Fallback operator: return `y` when `x` is NULL, otherwise `x`.
`%||%` <- function(x, y) if (is.null(x)) y else x

