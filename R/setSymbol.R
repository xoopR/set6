setSymbol <- function(set){
 if(!inherits(set,"character"))
    set = paste0(substitute(set))
  set = tolower(set)
  if (useUnicode()) {
    return(switch(set,
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
                  complex = "\u2102"
                  ))
  } else {
    return(switch(set,
                  naturals = "N",
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
                  complex = "C"
    ))
  }
}
