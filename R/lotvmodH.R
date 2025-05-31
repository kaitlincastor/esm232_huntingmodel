#' Lot. Voltera Model with hunting 
#'
#' Computes the rate of change of populations in a predator-prey interaction with hunting.
#' @param t Time (not used but required by ode solver)
#' @param pop Named list with two values: prey = number of prey, pred = number of predators
#' @param pars Named list of parameters: rprey, alpha, eff, pmort, hunt, minprey, K
#' @return List of rates of change for prey and predator

lotvmodH <- function(t, pop, pars) {
  with(as.list(c(pars, pop)), {
    
    # Only allow hunting when prey population exceeds threshold 
    actualhunt <- ifelse(prey >= minprey, hunt, 0)
    
    prey <- max(prey, 0)
    pred <- max(pred, 0)
    
      # Rate of change equations
    dprey <- rprey * (1 - prey / K) * prey - (alpha * prey * pred) - (actualhunt * prey)
    dpred <- eff * alpha * prey * pred - pmort * pred
    return(list(c(dprey, dpred)))
  })
}