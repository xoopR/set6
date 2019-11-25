#' @title Set of <%=toproper(set)%>
#' @description The mathematical set of <%=set%>, defined as <%=def%>. i.e. \deqn{<%=latexeqn%>}{<%=roxeqn%>} <%= if(exists("support")) support%>
#' @details <%= if(exists("deets")) deets%>
#' @section Constructor: <%=class%>$new(<%= if(exists("conargs")) conargs%>)
#' @section Constructor Arguments:
#' \tabular{lll}{
#'   **Argument** \tab **Type** \tab **Details** \cr
#'   <%= if(exists("arg1")) arg1%>
#'   <%= if(exists("arg2")) arg2%>
#'   <%= if(exists("arg3")) arg3%>
#'   <%= if(exists("arg4")) arg4%>
#'   <%= if(exists("arg5")) arg5%>
#'   <%= if(exists("arg6")) arg6%>
#'   <%= if(exists("arg7")) arg7%>
#'   <%= if(exists("arg8")) arg8%>
#'   <%= if(exists("arg9")) arg9%>
#'   <%= if(exists("arg10")) arg10%>
#'   }
#' @section Constructor Details: <%=constructorDets%>
#' @inheritSection Interval Public Fields
#' @inheritSection Interval Public Methods
#' @seealso [listSpecialSets]
#' @family SpecialSets
#' @return An R6 object of class <%=class%>.
#' @examples
#' <%=class%>$new()
