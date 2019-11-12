PowerSet <- R6::R6Class("PowerSet", inherit = SetWrapper)
PowerSet$set("private", ".power", 1)
PowerSet$set("active", "power", function() return(private$.power))
PowerSet$set("public", "initialize", function(set, power){
  lower = Tuple$new(rep(set$lower, power))
  upper = Tuple$new(rep(set$upper, power))
  type = "{}"
  symbol = paste(set$strprint(), power, sep ="^")
  super$initialize(setlist = setlist, lower = lower, upper = upper, type = type, symbol = symbol)
})

power <- function(x, power){
  product(x, x)
}
power.Set <- function(x, power){
  y = x
  for (i in 1:power)
   y = product(x, y)
  return(y)
}
power.Interval <- function(x, power){
  PowerSet$new(x, power)
}
power.ConditionalSet <- function(x, power){
  return(x)
}
power.PowerSet <- function(x, power){
  PowerSet$new(x$wrappedSets[[1]], x$power * power)
}

`^.Set` <- function(x, power){
  power(x, power)
}
