#' @templateVar class fit
#' @templateVar typeof prediction
#' @template title_desc_sinterval
#'
#' @param mod An model to simulate from.
#' @template params_sinterval
#' @param ... Additional arguments to prediction simulation method.
#' @return A [tibble::tibble()] with information about simulate values.
#'
#'
#' @export
add_predicted_sims <- function(newdata,
                         mod,
                         n_sims = 1000,
                         seed = NULL,
                        ...) {
  UseMethod("add_predicted_sims", mod)
}


#' @templateVar class fit
#' @templateVar typeof fitted
#' @template title_desc_sinterval
#'
#' @param mod An model to simulate from.
#' @template params_sinterval
#' @param ... Additional arguments to prediction simulation method.
#'
#'
#' @export
add_fitted_sims <- function(newdata,
                             mod,
                             n_sims = 1000,
                             seed = NULL,
                             ...) {
  UseMethod("add_fitted_sims", mod)
}
