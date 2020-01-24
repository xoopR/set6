operation_cleaner <- function(sets, operation_class, nest){
  assertSetList(sets)

  sets = sets[sapply(sets, function(x) try(testEmpty(x), silent = TRUE)) != "TRUE"]

  if (!nest) {
    sets = unlist(lapply(sets, function(x){
    wraps = x$wrappedSets
    if(is.null(wraps))
      return(x)
    else
      return(wraps)
  }))
  }

  classes = sapply(sets, getR6Class)

  interval = grepl("Interval", classes)
  if(any(interval))
    sets[interval] = lapply(sets[interval], function(x){
      return(ifnerror(as.Set(x), error = x))
    })

  fuzzy = grepl("Fuzzy", classes)
  if(any(fuzzy) & !all(fuzzy))
    sets[fuzzy] = lapply(sets[fuzzy], crispify)

  return(sets)
}
