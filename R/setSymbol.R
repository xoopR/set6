setSymbol <- function(set, zero) {
  set <- tolower(set)
  zero_str <- ifelse(zero, "0", "")
  uni <- ifelse(useUnicode(), "unicode", "ascii")
  zer <- ifelse(zero, "zero", "non_zero")
  lst <- symbol_env[[uni]][[zer]]
  as.character(lst[names(lst) %in% set])
}

symbol_env <- new.env()
local({
  symbol_env$unicode$zero <- list(
    naturals = "\u21150",
    posnaturals = "\u2115+",
    integers = "\u2124",
    posintegers = "\u21240+",
    negintegers = "\u21240-",
    rationals = "\u211A",
    posrationals = "\u211A0+",
    negrationals = "\u211A0-",
    reals = "\u211D",
    posreals = "\u211D0+",
    negreals = "\u211D0-",
    extendedreals = "\u211D \u222A {-\u221E, +\u221E}",
    complex = "\u2102",
    logicals = "{TRUE, FALSE}",
    universal = "\U1D54D"
  )

  symbol_env$unicode$non_zero <- list(
    naturals = "\u21150",
    posnaturals = "\u2115+",
    integers = "\u2124",
    posintegers = "\u2124+",
    negintegers = "\u2124-",
    rationals = "\u211A",
    posrationals = "\u211A+",
    negrationals = "\u211A-",
    reals = "\u211D",
    posreals = "\u211D+",
    negreals = "\u211D-",
    extendedreals = "\u211D \u222A {-\u221E, +\u221E}",
    complex = "\u2102",
    logicals = "{TRUE, FALSE}",
    universal = "\U1D54D"
  )

  symbol_env$ascii$zero <- list(
    naturals = "N0",
    posnaturals = "N+",
    integers = "Z",
    posintegers = "Z0+",
    negintegers = "Z0-",
    rationals = "Q",
    posrationals = "Q0+",
    negrationals = "Q0-",
    reals = "R",
    posreals = "R0+",
    negreals = "R0-",
    extendedreals = "R U {-Inf, +Inf}",
    complex = "C",
    logicals = "{TRUE, FALSE}",
    universal = "V"
  )

  symbol_env$ascii$non_zero <- list(
    naturals = "N0",
    posnaturals = "N+",
    integers = "Z",
    posintegers = "Z+",
    negintegers = "Z-",
    rationals = "Q",
    posrationals = "Q+",
    negrationals = "Q-",
    reals = "R",
    posreals = "R+",
    negreals = "R-",
    extendedreals = "R U {-Inf, +Inf}",
    complex = "C",
    logicals = "{TRUE, FALSE}",
    universal = "V"
  )
})