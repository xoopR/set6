#' @importFrom R6 R6Class

.onLoad = function(libname, pkgname) {
  options(set6.unicode = l10n_info()$`UTF-8`)
}

.onUnload = function(libname, pkgname) {
  options(set6.unicode = NULL)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("\n-----------------------------")
  packageStartupMessage("\tset6 v",utils::packageVersion("set6"),
                        "\n\nGet started:\t?set6
Changelog:\tset6News()")
  packageStartupMessage("-----------------------------\n")
}

