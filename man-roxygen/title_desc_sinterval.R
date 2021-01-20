#' @title Generate <%= typeof %> simulations from a <%= class %> model
#'
#' @description Generate simulations
#' from a <%= class %> model incorporating either error in
#' <%= typeof %> error. Simulations  explore the possible space
#' of what a model might predict rather than an interval for use
#' in comparison to Bayesian posteriors for non-Bayesian models. The
#' output format and functions draw inspiration from the
#' [tidybayes::tidybayes()] library and
#' [merTools::predictInterval()]
#'
#' @param newdata a data.frame of new data to predict
