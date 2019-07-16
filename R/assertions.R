makeChecks(assertionName = "SetInterval",
           cond = inherits(object, "SetInterval"),
           errormsg = paste(object, "is not an R6 SetInterval object"),
           pos = environment())

makeChecks(assertionName = "Set",
           cond = inherits(object, "Set"),
           errormsg = paste(object, "is not an R6 Set object"),
           pos = environment())

makeChecks(assertionName = "Tuple",
           cond = inherits(object, "Tuple"),
           errormsg = paste(object, "is not an R6 Tuple object"),
           pos = environment())

makeChecks(assertionName = "Interval",
           cond = inherits(object, "Interval"),
           errormsg = paste(object, "is not an R6 Interval object"),
           pos = environment())
