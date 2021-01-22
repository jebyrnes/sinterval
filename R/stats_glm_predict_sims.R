#' @templateVar class glm
#' @templateVar typeof fitted
#' @template title_desc_sinterval
#'
#' @param mod An lm model to simulate from.
#' @template params_sinterval
#' @param weights numeric, optional argument for binomial models that need a number of trials
#' @template params_unused_dots
#' @return A [tibble::tibble()] with information about simulate values.
#'
#' @family glm
#'
#' @examples
#'# Gamma
#' clotting <- data.frame(
#'    u = c(5,10,15,20,30,40,60,NA,100),
#'    lot1 = c(118,58,42,35,27,25,21,19,18),
#'    lot2 = c(69,35,26,21,18,16,13,12,12))
#'
#' mod <- glm(lot1 ~ log(u) + lot2, data = clotting, family = Gamma)
#'
#' sims_fit <- add_fitted_sims(clotting, mod)
#'
#' head(sims_fit)
#'
#'# Binomial
#'# example from Venables and Ripley (2002, pp. 190-2.)
#' ldose <- rep(0:5, 2)
#' numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
#' sex <- factor(rep(c("M", "F"), c(6, 6)))
#' SF <- cbind(numdead, numalive = 20-numdead)
#' budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
#'
#' dat <- data.frame(sex = factor(c("M", "F", "M", "F")),
#'                   ldose = c(0,0,5,5))
#'
#' sims_fit_b <- add_fitted_sims(dat, budworm.lg)
#'
#' head(sims_fit_b)
#' @export
add_fitted_sims.glm <- function(newdata,
                                mod,
                                n_sims = 1000,
                                seed = NULL,
                                weights = 1,
                                ...){
  add_predicted_sims.glm(newdata, mod, n_sims,
                         seed, weights, type = "fit", ...)
}

#' @templateVar class glm
#' @templateVar typeof fitted
#' @template title_desc_sinterval
#'
#' @param mod An lm model to simulate from.
#' @template params_sinterval
#' @param weights numeric, optional argument for binomial models that need a number of trials
#' @param type Character defining if we are looking at fit or predict intervals.
#' @template params_unused_dots
#' @return A [tibble::tibble] with information about simulate values.
#'
#' @family glm
#'
#' @examples
#'# Gamma
#' clotting <- data.frame(
#'    u = c(5,10,15,20,30,40,60,NA,100),
#'    lot1 = c(118,58,42,35,27,25,21,19,18),
#'    lot2 = c(69,35,26,21,18,16,13,12,12))
#'
#' mod <- glm(lot1 ~ log(u) + lot2, data = clotting, family = Gamma)
#'
#' sims_pred <- add_predicted_sims(clotting, mod)
#'
#' head(sims_pred)
#'
#'# Binomial
#'# example from Venables and Ripley (2002, pp. 190-2.)
#' ldose <- rep(0:5, 2)
#' numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
#' sex <- factor(rep(c("M", "F"), c(6, 6)))
#' SF <- cbind(numdead, numalive = 20-numdead)
#' budworm.lg <- glm(SF ~ sex*ldose, family = binomial)
#'
#' dat <- data.frame(sex = factor(c("M", "F", "M", "F")),
#'                   ldose = c(0,0,5,5))
#' sims_pred_b <- add_predicted_sims(dat, budworm.lg, weights = 20)
#'
#' head(sims_pred_b)
#' @export
add_predicted_sims.glm <- function(newdata,
                                  mod,
                                  n_sims = 1000,
                                  seed = NULL,
                                  weights = 1,
                                  type = c("predict", "fit", "linpred"),
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

  #calculate y
  Y <- X %*% t(stats::coef(coef_sims))

  if(type != "linpred"){
    Y <- mod$family$linkinv(Y)
  }

  fam <- ifelse(inherits(mod, "glm"), mod$family$family, "gaussian")

  # use the family info to add the correct residual if
  # this is a prediction
  if(type == "predict"){
    Y <- add_glm_residuals(Y, fam, coef_sims, mod, weights)
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
  sapply(sigma_vec, function(s) rnorm(n, 0, s))
}

add_glm_residuals <- function(Y, fam, sims, mod, weights = 1){

  #simuations taken from each family's simulate() function
  switch(fam,

         "gaussian" = {
           stats::rnorm(length(Y), Y, arm::sigma.hat(sims))
         },

         "Gamma" = {
           shape <- MASS::gamma.shape(mod)$alpha
           stats::rgamma(length(Y), shape = shape, rate = shape/Y)
         },

         "poisson" = {
           stats::rpois(length(Y), Y)
           },

         "quasipoisson" = {
           stats::rnorm(length(Y), Y, arm::sigma.hat(sims)^2) #account for overdispersion
         },

         "quasibinomial" = {stop("quasibinomial function for sinterval not implemented yet.")},

         "quasi" = {stop("General quasi function for sinterval not implemented.")},

         "binomial" = {
           wts <- rep(weights, length(Y)*length(weights))
           stats::rbinom(length(Y), prob = Y, size = wts)
           },

         "inverse.gaussian" = {
           SuppDists::rinvGauss(length(Y), nu = Y, lambda = 1/summary(mod)$dispersion)}
         )

}

