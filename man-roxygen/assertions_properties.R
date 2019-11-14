#' @title assert/check/test/<%=property%>
#'
#' @description Validation checks to test if a given object is <%=tolower(property)%>.
#'
#' @param object object to test
#' @param errormsg error message to overwrite default if check fails
#'
#' @return If check passes then `assert` returns `object` invisibly and `test`/`check`
#' return `TRUE`. If check fails, `assert` stops code with error, `check` returns
#' an error message as string, and `test` returns `FALSE`.
#'
#' @examples
#' test<%=property%>(<%=test1%>)
#' test<%=property%>(<%=test2%>)
