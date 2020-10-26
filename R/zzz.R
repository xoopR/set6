#' @importFrom R6 R6Class

# nocov start
.onLoad <- function(libname, pkgname) {
  options(set6.unicode = l10n_info()$`UTF-8`)
}

.onUnload <- function(libname, pkgname) {
  options(set6.unicode = NULL)
}

# nocov end
