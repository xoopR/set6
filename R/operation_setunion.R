#' @name setunion
#' @param ... [Set]s
#' @param x,y [Set]
#' @title Union of Sets
#' @return An object inheriting from [Set] containing the union of supplied sets.
#' @description Returns the union of objects inheriting from class [Set].
#' @details The union of \eqn{N} sets, \eqn{X1, ..., XN}, is defined as the set of elements that exist
#' in one or more sets,
#' \deqn{U = \{x : x \epsilon X1 or x \epsilon X2 or ... or x \epsilon XN\}}{U = {x : x \epsilon X1 or x \epsilon X2 or ... or x \epsilon XN}}
#'
#' The union of multiple [ConditionalSet]s is given by combining their defining functions by an
#' 'or', `|`, operator. See examples.
#'
#' @family operators
#' @examples
#' # union of Sets
#'
#' Set$new(-2:4) + Set$new(2:5)
#' setunion(Set$new(1,4,"a"), Set$new("a", 6))
#' Set$new(1,2) + Set$new("a", 1i) + Set$new(9)
#'
#' # union of intervals
#'
#' Interval$new(1, 10) + Interval$new(5, 15) + Interval$new(20, 30)
#' Interval$new(1, 2, type = "()") + Interval$new(2, 3, type = "(]")
#' Interval$new(1, 5, class = "integer") +
#'     Interval$new(2, 7, class = "integer")
#'
#' # union of mixed types
#'
#' Set$new(1:10) + Interval$new(5, 15)
#' Set$new(1:10) + Interval$new(5, 15, class = "integer")
#' Set$new(5,7) | Tuple$new(6, 8, 7)
#'
#' # union of FuzzySet
#' FuzzySet$new(1, 0.1, 2, 0.5) + Set$new(2:5)
#'
#' # union of conditional sets
#'
#' ConditionalSet$new(function(x, y) x >= y) +
#'     ConditionalSet$new(function(x, y) x == y) +
#'     ConditionalSet$new(function(x) x == 2)
#'
#' # union of special sets
#' PosReals$new() + NegReals$new()
#' Set$new(-Inf, Inf) + Reals$new()
#'
#' @export
setunion <- function(...){
  sets = list(...)

  sets = operation_cleaner(sets, "UnionSet", nest = FALSE)

  classes = sapply(sets, getR6Class)
  if ("PosReals" %in% classes & "NegReals" %in% classes){
    sets = c(sets, Reals$new())
    sets = sets[-match(c("PosReals", "NegReals"), classes)]
  } else if ("PosRationals" %in% classes & "NegRationals" %in% classes){
    sets = c(sets, Rationals$new())
    sets = sets[-match(c("PosRationals", "NegRationals"), classes)]
  } else if ("PosIntegers" %in% classes & "NegIntegers" %in% classes){
    sets = c(sets, Integers$new())
    sets = sets[-match(c("PosIntegers", "NegIntegers"), classes)]
  }

  classes = sapply(sets, getR6Class)

  if ("Reals" %in% classes & "{-Inf, Inf}" %in% rsapply(sets, strprint)){
    sets = c(sets, ExtendedReals$new())
    sets = sets[-c(match("Reals", classes), match("{-Inf, Inf}", rsapply(sets, strprint)))]
  }

  if(length(sets) == 0)
    return(Set$new())
  else if(length(sets) == 1)
    return(sets[[1]])

  if(length(unique(rsapply(sets, strprint))) == 1)
    return(sets[[1]])

  rm_ind = c()
  for(i in 1:length(sets)){
    for(j in 1:length(sets)){
      if(i != j){
        # separate subset and proper subset to prevent both sets being removed due to equality
        if(sets[[i]] < sets[[j]] | (sets[[i]] == sets[[j]] & i < j))
          rm_ind = c(rm_ind, i)
      }
    }
  }
  if(!is.null(rm_ind))
    sets = sets[-rm_ind]

  if(length(sets) == 0)
    return(Set$new())
  else if(length(sets) == 1)
    return(sets[[1]])

  classes = sapply(sets, getR6Class)
  # hacky fix for SpecialSets
  classes[sapply(sets, function(x) inherits(x, "Interval"))] = "Interval"

  conditionals = fuzzies = intervals = crisps = NULL

  if(any(grepl("ConditionalSet", classes)))
    conditionals = .union_conditionalset(sets[grepl("ConditionalSet", classes)])
  if(any(grepl("Fuzzy", classes)))
    fuzzies = .union_fuzzyset(sets[grepl("Fuzzy", classes)])
  if(any(grepl("Interval", classes)))
    intervals = .union_interval(sets[grepl("Interval", classes)])
  if(any(classes %in% c("Set" , "Tuple")))
    crisps = .union_set(sets[classes %in% c("Set" , "Tuple")])


  sets = c(crisps, intervals, conditionals, fuzzies)

  if(length(sets) == 1)
    return(sets[[1]])
  else
    return(UnionSet$new(sets))
}

.union_set <- function(sets){
  if(length(sets) == 1)
    return(sets[[1]])

  if(any(grepl("Set", sapply(sets, getR6Class))))
    return(Set$new(rsapply(sets, elements, active = TRUE)))
  else
    return(Tuple$new(rsapply(sets, elements, active = TRUE)))
}
.union_interval <- function(sets){
  if(length(sets) == 1)
    return(sets[[1]])

  rm = c()
  sets = sets[order(rsapply(sets, lower, active = TRUE))]
  for(i in 2:length(sets)){
    if(sets[[i]]$lower > sets[[i-1]]$lower & sets[[i]]$lower < sets[[i-1]]$upper){
      sets[[i]] = Interval$new(sets[[i-1]]$lower, sets[[i]]$upper,
                               type = paste0(substr(sets[[i-1]]$type,1,1),
                                             substr(sets[[i]]$type,2,2)))
      rm = c(rm, i - 1)
    }
  }
  if(!is.null(rm))
    sets = sets[-rm]

  return(sets)
#
#
#   # function to seee which overlap,add these into a single interval then UnionSet the rest
#     if (x$upper > y$lower & x$lower < y$lower)
#       return(Interval$new(x$lower, y$upper,
#                           type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
#     else if(y$upper > x$lower & y$lower < x$lower)
#       return(Interval$new(y$lower, x$upper,
#                           type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
#     else if(x$upper < y$lower)
#       return(UnionSet$new(setlist = list(x,y), lower = x$lower, upper = y$upper,
#                        type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
#     else if(y$upper < x$lower)
#       return(UnionSet$new(setlist = list(y,x), lower = y$lower, upper = x$upper,
#                        type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
#     else if(y$upper == x$lower){
#       if(!testClosedAbove(y) & !testClosedBelow(x))
#         return(UnionSet$new(setlist = list(x,y), lower = y$lower, upper = x$upper,
#                          type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
#       else
#         return(Interval$new(y$lower, x$upper,
#                             type = paste0(substr(y$type,1,1),substr(x$type,2,2))))
#     } else if (x$upper == y$lower){
#       if(!testClosedAbove(x) & !testClosedBelow(y))
#         return(UnionSet$new(setlist = list(x,y), lower = x$lower, upper = y$upper,
#                          type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
#       else
#         return(Interval$new(x$lower, y$upper,
#                             type = paste0(substr(x$type,1,1),substr(y$type,2,2))))
#     }
}
.union_fuzzyset <- function(sets){
  if(length(sets) == 1)
    return(sets[[1]])

  if(any(grepl("FuzzySet", sapply(sets, getR6Class))))
    return(FuzzySet$new(elements = rsapply(sets, elements, active = TRUE),
                        membership = rsapply(sets, membership)))
  else
    return(FuzzyTuple$new(elements = rsapply(sets, elements, active = TRUE),
                        membership = rsapply(sets, membership)))
}
.union_conditionalset <- function(sets){
  if(length(sets) == 1)
    return(sets[[1]])

  condition = function(){}
  names = unique(names(unlist(lapply(sets,function(x) formals(x$condition)))))
  formals <- rep(list(bquote()), length(names))
  names(formals) = names
  formals(condition) = formals
  body(condition) = substitute(bx | by, list(bx = body(sets[[1]]$condition),
                                             by = body(sets[[2]]$condition)))
  if(length(sets) > 2){
    for(i in 3:length(sets))
      body(condition) = substitute(bx | by, list(bx = body(condition),
                                                 by = body(sets[[i]]$condition)))
  }

  # in future updates we can change this so the union of the argument classes is kept
  # not just the argclass of x
  class = unlist(rlapply(sets, class, active = TRUE))[!duplicated(names(unlist(rlapply(sets, class, active = TRUE))))]
  return(ConditionalSet$new(condition = condition, argclass = class))
}

#' @rdname setunion
#' @export
`+.Set` <- function(x, y){
  setunion(x, y)
}

#' @rdname setunion
#' @export
'|.Set' <- function(x, y){
  setunion(x, y)
}
