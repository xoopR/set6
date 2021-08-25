assertThat <- function(x, cond, errormsg) {
  if (cond) {
    invisible(x)
  } else {
    stop(errormsg)
  }
}
checkThat <- function(cond, errormsg) {
  if (cond) {
    return(TRUE)
  } else {
    return(errormsg)
  }
}
testThat <- function(cond) {
  if (cond) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
isThat <- function(cond) {
  return(testThat(cond))
}

makeChecks <- function(assertionName, cond, errormsg, args,
                       pos = -1) {

  if (missing(args)) {
    args <- c(alist(object = ), list(errormsg = errormsg)) # nolint
  }

  cond <- substitute(cond)
  # errormsg = substitute(errormsg)
  value <- function() {}
  formals(value) <- args
  body(value) <- substitute(assertThat(object, arg1, errormsg), list(arg1 = cond))
  assign(paste0("assert", assertionName),
    value = value,
    pos = pos
  )

  body(value) <- substitute(checkThat(arg1, errormsg), list(arg1 = cond))
  assign(paste0("check", assertionName),
    value = value,
    pos = pos
  )

  body(value) <- substitute(testThat(arg1), list(arg1 = cond))
  assign(paste0("test", assertionName),
    value = value,
    pos = pos
  )
}


ifnerror <- function(expr, noerror = NULL, error = NULL, silent = T, stopwarn = "warn",
                     errormsg = "Error not Nerror!") {
  x <- try(expr, silent)
  if (inherits(x, "try-error")) {
    if (is.null(error)) {
      stopwarn(stopwarn, errormsg)
    } else {
      return(error)
    }
  } else {
    if (is.null(noerror)) {
      noerror <- x
    }
    return(noerror)
  }
}

stopwarn <- function(error = c("warn", "stop"), error.msg) {
  error <- match.arg(error)

  if (error == "stop") {
    stop(error.msg, call. = FALSE)
  } else {
    warning(error.msg, call. = F)
    return(NULL)
  }
}
testMessage <- function(expr) {
  if (inherits(tryCatch(expr, message = function(m) m), "message")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

modal <- function(data) {
  tab <- table(unlist(data))
  modal <- as.numeric(names(tab)[tab == max(tab)])
  return(modal)
}

toproper <- function(str) {
  unlist(lapply(strsplit(str, " ", TRUE), function(x) {
    paste(toupper(substr(x, 1, 1)),
      tolower(substr(x, 2, 10000)),
      sep = "", collapse = " "
    )
  }))
}


crispify <- function(x) {
  if (testCrisp(x)) {
    return(x)
  } else if (testFuzzyTuple(x)) {
    return(as.Tuple(x))
  } else {
    return(as.Set(x))
  }
}
fuzzify <- function(x) {
  if (testFuzzy(x)) {
    return(x)
  } else if (testTuple(x)) {
    return(as.FuzzyTuple(x))
  } else if (object_class(x) == "Set") {
    return(as.FuzzySet(x))
  } else {
    stop(as.character(x), " cannot be fuzzified.")
  }
}

listify <- function(x) {
  if (!checkmate::testList(x)) {
    if (inherits(x, "R6")) {
      x <- list(x)
    } else {
      x <- as.list(x)
    }
  }

  return(x)
}
returner <- function(x, all) {
  if (length(x) == 1 & class(x)[1] == "list") {
    x <- x[[1]]
  }

  if (all) {
    return(all(x))
  } else {
    return(x)
  }
}
sortlist <- function(x, decreasing = FALSE, ...) {
  x[order(names(x), decreasing = decreasing, ...)]
}
