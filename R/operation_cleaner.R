operation_cleaner <- function(sets, operation_class, nest, simplify = TRUE) {
  assertSetList(sets)

  sets <- sets[sapply(sets, function(x) try(testEmpty(x), silent = TRUE)) != "TRUE"]

  if (!nest) {
    sets <- unlist(lapply(sets, function(x) {
      wraps <- try(x$wrappedSets, silent = TRUE)
      if (class(wraps)[1] == "try-error") {
        return(x)
      } else if (is.null(wraps)) {
        return(x)
      } else {
        return(wraps)
      }
    }))
  }

  if (simplify) {
    classes <- sapply(sets, getR6Class)
    set <- grepl("Set", classes)
    interval <- grepl("Interval", classes)

    if (all(classes %in% c("Set", "Interval"))) {
      # try converting all intervals to sets
      if (any(sapply(sets[interval], function(x) x$properties$countability == "uncountable"))) {
        sets[set] <- lapply(sets[set], function(el) {
          suppressWarnings(return(ifnerror(as.Interval(el), error = el)))
        })
        return(sets)
      }
    }

    classes <- sapply(sets, getR6Class)
    interval <- grepl("Interval", classes)
    fuzzy <- grepl("Fuzzy", classes)

    if (any(interval)) {
      sets[interval] <- lapply(sets[interval], function(x) {
        return(ifnerror(as.Set(x), error = x))
      })
    }

    if (any(fuzzy) & !all(fuzzy)) {
      sets[fuzzy] <- lapply(sets[fuzzy], crispify)
    }
  }

  return(sets)
}
