setSymbol <- function(set){
  x = try(class(set),silent = T)
  if(inherits(x, "try-error"))
    set = paste0(substitute(set))
  else if(!inherits(set,"character"))
    set = paste0(substitute(set))
  set = tolower(set)
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
}
