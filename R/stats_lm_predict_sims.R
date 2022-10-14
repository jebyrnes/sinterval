#' @templateVar class lm
#' @templateVar typeof fitted
#' @template title_desc_sinterval
#'
#' @param mod A [lm] model to simulate from.
#' @template params_sinterval
#' @template params_unused_dots
#' @return A [tibble::tibble()] with information about simulate values.
#'
#' @family lm
#'
#' @examples
#' clotting <- data.frame(
#'    u = c(5,10,15,20,30,40,60,NA,100),
#'    lot1 = c(118,58,42,35,27,25,21,19,18),
#'    lot2 = c(69,35,26,21,18,16,13,12,12))
#'
#' mod <- lm(lot1 ~ log(u) + lot2, data = clotting)
#'
#' sims_fit <- add_fitted_sims(clotting, mod)
#'
#' head(sims_fit)
#'
#' @export
add_fitted_sims.lm <- function(newdata,
                               mod,
                               n_sims = 1000,
                               seed = NULL,
                               ...){
  add_predicted_sims.lm(newdata, mod, n_sims, seed,
                        type = "fit", ...)
}

#' @templateVar class lm
#' @templateVar typeof predicted
#' @template title_desc_sinterval
#'
#' @param mod An lm model to simulate from.
#' @template params_sinterval
#' @param type Character defining if we are looking at fit or predict intervals.
#' @template params_unused_dots
#' @return A [tibble::tibble()] with information about simulate values.
#'
#' @family lm
#' @examples
#' clotting <- data.frame(
#'    u = c(5,10,15,20,30,40,60,NA,100),
#'    lot1 = c(118,58,42,35,27,25,21,19,18),
#'    lot2 = c(69,35,26,21,18,16,13,12,12))
#'
#' mod <- lm(lot1 ~ log(u) + lot2, data = clotting)
#'
#' sims_pred <- add_predicted_sims(clotting, mod)
#'
#' head(sims_pred)
#'
#' @export
add_predicted_sims.lm <- function(newdata,
                         mod,
                         n_sims = 1000,
                         seed = NULL,
                         type = c("predict", "fit"),
                         ...) {

  if(!is.null(seed)) set.seed(seed)
  type <- type[1]


  #make a model matrix with the data
  resp <- as.character(stats::formula(mod))[[2]]

  is_resp <- is.null(newdata[[resp]])
  if(is_resp){
    newdata[[resp]] <- NA
  }

  X <- stats::model.matrix(mod,
                           data = newdata,
                           na.action = NULL)

  if(is_resp){
    newdata <- newdata[ -ncol(newdata)] #it was added as the last col
  }

  #get the coefficients
  coef_sims <- arm::sim(mod, n.sims = n_sims)
  resmat <- get_res_mat(arm::sigma.hat(coef_sims), nrow(X))

  #calculate y
  Y <- X %*% t(stats::coef(coef_sims))

  if("predict" %in% type){
    Y <- Y+resmat
  }

  yvar <- names(stats::model.frame(mod))[1]

  y_all <- cbind(newdata, Y)

  Y_long <- y_all %>%
   # tidyr::as_tibble(.name_repar = "check_unique") %>%
    tidyr::pivot_longer(
      cols = -names(newdata),
      names_to = ".sim",
      values_to = paste0(yvar,"_", type)
    )

  return(Y_long)




}


get_res_mat <- function(sigma_vec, n){
  sapply(sigma_vec, function(s) stats::rnorm(n, 0, s))
}
