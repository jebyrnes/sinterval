#' @templateVar class psem
#' @templateVar typeof fitted
#' @template title_desc_sinterval
#'
#' @param mod An [piecewiseSEM::psem()] object to simulate from.
#' @template params_sinterval
#' @template params_unused_dots
#' @return A [tibble::tibble()] with information about simulate values.
#'
#' @family psem
#'
#' @examples
#'
#'\dontrun{
#'
#'library(piecewiseSEM)
#'data(keeley)
#'mod <- psem(
#'  lm(abiotic ~ distance, data=keeley),
#'  lm(rich ~ abiotic + hetero, data=keeley),
#'  lm(hetero ~ distance, data=keeley),
#'  data = keeley)
#'
#'newdat <- data.frame(distance=c(30, 50))
#'
#'new_fitted_sims <- newdat %>%
#'  add_fitted_sims(mod)
#'
#'head(new_fitted_sims)
#'}
#' @export
add_fitted_sims.psem <- function(newdata,
                               mod,
                               n_sims = 1000,
                               seed = NULL,
                               ...){
  add_predicted_sims.psem(newdata, mod, n_sims, seed,
                        type = "fit", ...)
}

#' @templateVar class psem
#' @templateVar typeof predicted
#' @template title_desc_sinterval
#'
#' @param mod An [piecewiseSEM::psem()] object to simulate from.
#' @template params_sinterval
#' @param type Character defining if we are looking at fit or predict intervals.
#' @template params_unused_dots
#' @return A [tibble::tibble()] with information about simulate values.
#'
#' @family psem
#'
#' @examples
#'
#'\dontrun{
#'
#'library(piecewiseSEM)
#'data(keeley)
#'mod <- psem(
#'  lm(abiotic ~ distance, data=keeley),
#'  lm(rich ~ abiotic + hetero, data=keeley),
#'  lm(hetero ~ distance, data=keeley),
#'  data = keeley)
#'
#'newdat <- data.frame(distance=c(30, 50))
#'
#'new_predicted_sims <- newdat %>%
#'  add_predicted_sims(mod)
#'
#'head(new_predicted_sims)
#'}
#' @export
add_predicted_sims.psem <- function (newdata,
                                  mod,
                                  n_sims = 1000,
                                  seed = NULL,
                                  type = c("predict", "fit"),
                                  ...){

  type <- type[1]

  #get a sorted DAG for simulation
  mod_sorted <- piecewiseSEM::getSortedPsem(mod)
  mod_sorted <- removeData(mod_sorted, formulas = 1)

  #holding on to for later
  #out_dat <- newdata

  #iterate over each piece
  #need to find a better way to do this, memory-wise
  resp <- rep(NA, length(mod_sorted))

  for(i in 1:length(mod_sorted)){
    #get the response
    resp[i] <- as.character(stats::formula(mod_sorted[[i]]))[[2]]

    newdata <- psem_sim_reduce(newdata, mod_sorted[[i]],
                               n_sims, seed, type, ...)
  }

  #fix colmn names - add predicted or fitted back to
  #those variables, and lop _old off of variables from initial data
  #frame
  names(newdata)[names(newdata) %in% resp] <-
    paste0(names(newdata)[names(newdata) %in% resp], "_", type)

  is_old <- grepl("_old$", names(newdata))
    if(sum(is_old)>0){
    names(newdata) <- gsub("_old$", "", names(newdata))
  }

  #data out!
  newdata

}


#' do the work of simulations
#'
#' @keywords internal
#'
#iterate over each relationship and get the appropriate
#prediction or fit interval
psem_sim_reduce <- function(newdata, one_piece,
                            n_sims, seed, type, ...){

  # check and see if we have any variables with the same name as
  # response variables. Add _old to them
  #
  resp <- as.character(stats::formula(one_piece))[[2]]

  if(resp %in% names(newdata)){
    names(newdata)[names(newdata)==resp] <- paste0(resp, "_old")
  }

  #modify the sim column if there
  if(".sim" %in% names(newdata)){
    names(newdata)[names(newdata)==".sim"] <- ".sim_old"
  }

  #get the prediction/fit simulations
  dat <- add_predicted_sims(newdata, one_piece,
                            n_sims, seed, type, ...)

  #remove bits from sinterval on the predicted/fitted response variables
  names(dat) <- gsub("(_predict|_fit)$", "", names(dat))

  #if this is a downstream simulation....
  if (".sim_old" %in% names(dat)){
    #for each sim from previous simulations
    #sift down so that we do not have one simulation dominating
    dat <- dat[dat$`.sim` == dat$`.sim_old`,]
    dat <- dat[,-which(names(dat) == ".sim_old")] #don't need for next iter
  }

  return(dat)

}


#' Remove data from the model list
#'
#' formulas = 0, keep everything
#' formulas = 1, remove all formulas including correlated errors
#' formulas = 2, remove only formula but keep correlated errors
#' formulas = 3, remove correlated errors but keep formula
#'
#' @keywords internal
#'
removeData <- function(modelList, formulas = 0) {

  remove <- c("character", "matrix", "data.frame", "SpatialPointsDataFrame", "comparative.data")

  if(formulas == 1) remove <- c(remove, "formula", "formula.cerror")

  if(formulas == 2) remove <- c(remove, "formula")

  if(formulas == 3) remove <- c(remove, "formula.cerror")

  modelList[!sapply(modelList, function(x) any(class(x) %in% remove))]

}
