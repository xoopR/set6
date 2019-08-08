makeChecks(assertionName = "SetInterval",
           cond = inherits(object, "SetInterval"),
           errormsg = "This is not an R6 SetInterval object",
           pos = environment())

makeChecks(assertionName = "Set",
           cond = inherits(object, "Set"),
           errormsg = "This is not an R6 Set object",
           pos = environment())

makeChecks(assertionName = "Tuple",
           cond = inherits(object, "Tuple"),
           errormsg = "This is not an R6 Tuple object",
           pos = environment())

makeChecks(assertionName = "FuzzySet",
           cond = inherits(object, "FuzzySet"),
           errormsg = "This is not an R6 FuzzySet object",
           pos = environment())

makeChecks(assertionName = "Interval",
           cond = inherits(object, "Interval"),
           errormsg = "This is not an R6 Interval object",
           pos = environment())

makeChecks(assertionName = "BoundedAbove",
           cond = testSet(object) |  substr(object$type(),2,2) == "]",
           errormsg = "This is not bounded above",
           pos = environment())

makeChecks(assertionName = "BoundedBelow",
           cond = testSet(object) |  substr(object$type(),1,1) == "[",
           errormsg = "This is not bounded below",
           pos = environment())
