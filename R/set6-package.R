## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib set6, .registration = TRUE
## usethis namespace: end
NULL

#' set6: R6 Mathematical Sets Interface
#'
#' set6 upgrades the `{sets}` package to R6. Many forms of mathematical sets are implemented,
#' including (countably finite) sets, tuples, intervals (countably infinite or uncountable),
#' and fuzzy variants. Wrappers extend functionality by allowing symbolic representations of
#' complex operations on sets, including unions, (cartesian) products, exponentiation, and
#' differences (asymmetric and symmetric).
#'
#' The main features of set6 are:
#'
#' \itemize{
#' \item Object-oriented programming, which allows a clear inheritance structure for Sets, Intervals,
#' Tuples, and other variants.
#' \item Set operations and wrappers for both explicit and symbolic representations for algebra of sets.
#' \item Methods for assertions and comparison checks, including subsets, equality, and containedness.
#' }
#'
#' To learn more about set6, start with the set6 vignette:
#'
#' \code{vignette("set6", "set6")}
#'
#' And for more advanced usage see the complete tutorials at
#'
#'\href{https://github.com/xoopR/set6}{https://github.com/xoopR/set6}
#'
"_PACKAGE"
