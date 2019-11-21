assertThat <- function(x, cond, errormsg){
  if(cond)
    invisible(x)
  else
    stop(errormsg)
}
checkThat <- function(cond, errormsg){
  if(cond)
    return(TRUE)
  else
    return(errormsg)
}
testThat <- function(cond){
  if(cond)
    return(TRUE)
  else
    return(FALSE)
}
isThat <- function(cond){
  return(testThat(cond))
}

makeChecks <- function(assertionName, cond, errormsg, args,
                       pos = -1){

  if(missing(args))
    args = c(alist(object = ), list(errormsg = errormsg))

  cond = substitute(cond)
  # errormsg = substitute(errormsg)
  value = function(){}
  formals(value) = args
  body(value) = substitute(assertThat(object,arg1,errormsg),list(arg1=cond))
  assign(paste0("assert",assertionName), value = value,
         pos = pos)

  body(value) = substitute(checkThat(arg1,errormsg),list(arg1=cond))
  assign(paste0("check",assertionName), value = value,
         pos = pos)

  body(value) = substitute(testThat(arg1),list(arg1=cond))
  assign(paste0("test",assertionName), value = value,
         pos = pos)
}

getR6Class <- function(object, classname = TRUE, n.par = 0, pos = -1){
  if(classname)
    return(get(class(object)[[n.par+1]], pos = pos)$classname)
  else
    return(get(class(object)[[n.par+1]], pos = pos))
}
ifnerror <- function(expr, noerror, error = NULL, silent = T){
  x = try(expr, silent)
  if(inherits(x, "try-error")){
    if(is.null(error) | error == "warn")
      stopwarn("warn", "Error not Nerror!")
    else if(error == "stop")
      stopwarn("stop", "Error not Nerror!")
    else
      error
  } else {
    noerror
  }
}

stopwarn <- function(error = "warn", error.msg){
  checkmate::assert(error == "warn", error == "stop",
                    .var.name = "'error' should be one of 'warn' or 'stop'.")
  if(error == "stop")
    stop(error.msg)
  else{
    warning(error.msg, call. = F)
    return(NULL)
  }
}
testMessage <- function(expr){
  if(inherits(tryCatch(expr, message = function(m) m), "message"))
    return(TRUE)
  else
    return(FALSE)
}

modal = function(data){
  tab = table(unlist(data))
  modal = as.numeric(names(tab)[tab==max(tab)])
  return(modal)
}

toproper = function(str){
  unlist(lapply(strsplit(str, " ", TRUE), function(x) paste(toupper(substr(x,1,1)),
                                                     tolower(substr(x,2,10000)),
                                                     sep = "", collapse = " ")))
}

rlapply = function(X, FUN, ..., active = FALSE){
  FUN = as.character(substitute(FUN))
  if(active)
    return(lapply(X, function(x) x[[FUN]]))
  else
    return(lapply(X, function(x) x[[FUN]](...)))
}
rsapply = function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE, active = FALSE){
  FUN = as.character(substitute(FUN))
  if(active)
    return(unlist(sapply(X, function(x) x[[FUN]], simplify = simplify, USE.NAMES = USE.NAMES)))
  else
    return(unlist(sapply(X, function(x) x[[FUN]](...), simplify = simplify, USE.NAMES = USE.NAMES)))
}

crispify = function(x){
  if (testCrisp(x))
    return(x)
  else if (testFuzzyTuple(x))
    return(as.Tuple(x))
  else
    return(as.Set(x))
}
fuzzify = function(x){
  if (testFuzzy(x))
    return(x)
  else if (testTuple(x))
    return(as.FuzzyTuple(x))
  else
    return(as.FuzzySet(x))
}
