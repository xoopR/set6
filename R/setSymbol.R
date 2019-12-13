setSymbol <- function(set, zero = FALSE){
 if(!inherits(set,"character"))
    set = paste0(substitute(set))
  set = tolower(set)
  zero_str = ifelse(zero, "0", "")
  if (useUnicode()) {
    return(switch(set,
                  naturals = "\u21150",
                  posnaturals = "\u2115+",
                  integers = "\u2124",
                  posintegers = paste0("\u2124",zero_str,"+"),
                  negintegers = paste0("\u2124",zero_str,"-"),
                  rationals = "\u211A",
                  posrationals = paste0("\u211A",zero_str,"+"),
                  negrationals = paste0("\u211A",zero_str,"-"),
                  reals = "\u211D",
                  posreals = paste0("\u211D",zero_str,"+"),
                  negreals = paste0("\u211D",zero_str,"-"),
                  extendedreals = "\u211D \u222A {-\u221E, +\u221E}",
                  complex = "\u2102"
                  ))
  } else {
    return(switch(set,
                  naturals = "N0",
                  posnaturals = "N+",
                  integers = "Z",
                  posintegers = paste0("Z",zero_str,"+"),
                  negintegers = paste0("Z",zero_str,"-"),
                  rationals = "Q",
                  posrationals = paste0("Q",zero_str,"+"),
                  negrationals = paste0("Q",zero_str,"-"),
                  reals = "R",
                  posreals = paste0("R",zero_str,"+"),
                  negreals = paste0("R",zero_str,"-"),
                  extendedreals = "R U {-Inf, +Inf}",
                  complex = "C"
    ))
  }
}
