power.Set <- function(x, power){
  symbol = paste0(x$strprint(),"^",power)
  lower = rep(x$lower(),power)
  upper = rep(x$upper(),power)

  Set$new(symbol = symbol, type = x$type(), lower = lower,
                  upper = upper, dimension = power)
}

`^.Set` <- function(x, power){
  power.Set(x, power)
}
