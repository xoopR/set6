#' @title assert/check/test/<%=class%>
#'
#' @description Validation checks to test if a given object is an R6 `<%=class%>`.
#'
#' @param object object to test
#' @param errormsg error message to overwrite default if check fails
#'
#' @return If check passes then `assert` returns `object` invisibly and `test`/`check`
#' return `TRUE`. If check fails, `assert` stops code with error, `check` returns
#' an error message as string, and `test` returns `FALSE`.
#'
#' @examples
#' test<%=class%>(Set$new(2, 3))
#' test<%=class%>(list(Set$new(2), Set$new(3)))
#' test<%=class%>(Tuple$new(2, 3))
#' test<%=class%>(Interval$new())
#' test<%=class%>(FuzzySet$new(2, 0.1))
#' test<%=class%>(FuzzyTuple$new(2, 0.1))
#' test<%=class%>(ConditionalSet$new(function(x) x == 0))
