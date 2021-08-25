operation_cleaner <- function(sets, operation_class, nest, simplify = TRUE) {
  assertSetList(sets)

  sets <- sets[vlapply(sets,
                      function(x) testConditionalSet(x) || !testEmpty(x))]

  if (!nest) {
    sets <- unlist(lapply(sets, function(x) {
      wraps <- try(x$wrappedSets, silent = TRUE)
      if (class(wraps)[1] == "try-error") {
        return(x)
      } else if (is.null(wraps)) {
        return(x)
      } else {
        if (inherits(x, "ExponentSet")) {
          if (x$power == "n") {
            return(x)
          }
        }
        return(wraps)
      }
    }))
  }

  if (simplify) {
    classes <- vcapply(sets, object_class)
    set <- grepl("Set", classes)
    interval <- grepl("Interval", classes)

    if (all(classes %in% c("Set", "Interval"))) {
      # try converting all intervals to sets
      if (any(vlapply(
        sets[interval],
        function(x) x$properties$countability == "uncountable"))) {

        sets[set] <- lapply(sets[set], function(el) {
          suppressWarnings(return(ifnerror(as.Interval(el), error = el)))
        })
        return(sets)
      }
    }

    classes <- vcapply(sets, object_class)
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
