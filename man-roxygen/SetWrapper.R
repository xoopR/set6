#' @name <%=(class)%>
#' @title Set of <%= toproper(operation)%>s
#' @description <%=(class)%> class for symbolic <%=(operation)%> of mathematical sets.
#' @section Constructor: `<%=constructor%>`
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
#'   `lower` \tab Set \tab Lower limit of set. \cr
#'   `upper` \tab Set \tab Upper limit of set. \cr
#'   `type` \tab character \tab Type of set braces. \cr
#'   }
#'
#' @section Constructor Details: It is not recommended to construct this class directly.
#'
#' @section Public Fields:
#' \tabular{ll}{
#'  **Field** \tab **Link** \cr
#'  <%= if(exists("field1")) field1%>
#'   <%= if(exists("field2")) field2%>
#'   <%= if(exists("field3")) field3%>
#'   <%= if(exists("field4")) field4%>
#'   <%= if(exists("field5")) field5%>
#'   <%= if(exists("field6")) field6%>
#'   <%= if(exists("field7")) field7%>
#'   <%= if(exists("field8")) field8%>
#'   <%= if(exists("field9")) field9%>
#'   <%= if(exists("field10")) field10%>
#'  `wrappedSets` \tab [wrappedSets] \cr
#'  `class` \tab [class] \cr
#'  `elements` \tab [elements] \cr
#'  `length` \tab [length] \cr
#'  `lower` \tab [lower] \cr
#'  `upper` \tab [upper] \cr
#'  `max` \tab [max] \cr
#'  `min` \tab [min] \cr
#'  `range` \tab [range] \cr
#'  `properties` \tab [properties] \cr
#'  `traits` \tab [traits] \cr
#'  `type` \tab [type] \cr
#'  `universe` \tab [universe]
#' }
#'
#' @section Public Methods:
#' \tabular{ll}{
#'   <%= if(exists("meth1")) meth1%>
#'   <%= if(exists("meth2")) meth2%>
#'   <%= if(exists("meth3")) meth3%>
#'   <%= if(exists("meth4")) meth4%>
#'   <%= if(exists("meth5")) meth5%>
#'   <%= if(exists("meth6")) meth6%>
#'   <%= if(exists("meth7")) meth7%>
#'   <%= if(exists("meth8")) meth8%>
#'   <%= if(exists("meth9")) meth9%>
#'   <%= if(exists("meth10")) meth10%>
#' **Comparison Methods** \tab **Link** \cr
#' `contains(x, all = FALSE, bound = NULL)` \tab [contains] \cr
#' `equals(x)` \tab [equals] \cr
#' `isSubset(x, proper = FALSE)` \tab [isSubset] \cr
#' \tab \cr \tab \cr \tab \cr
#' **Mathematical Methods** \tab \strong{Link} \cr
#' `complement()` \tab [complement] \cr
#' \tab \cr \tab \cr \tab \cr
#' **Representation Methods** \tab **Link** \cr
#' `strprint(n = 2)` \tab [strprint] \cr
#' `print(n = 2)` \tab [print] \cr
#' `summary(n = 2)` \tab [summary] \cr
#' }
#'
#' @details
#' The purpose of this class is to provide a symbolic representation for the <%=operation%> of sets that
#' cannot be represented in a simpler class. Whilst this is not an abstract class, it is not recommended to construct this class directly but via
#' the set operation methods: [union], [product], [power], [setdiff].
#'
#' @seealso
#' [UnionSet], [ExponentSet], [ProductSet], [DifferenceSet]
#'
#' @return R6 object of class <%=(class)%>, inheriting from SetWrapper.
