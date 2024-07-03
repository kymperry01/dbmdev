#' Stage-specific development parameters for the diamondback moth for Briere's model 2
#'
#' @param species Diamondback moth (species = "dbm")
#'
#' @return a Matrix
#' @export
#'
#' @examples
#' # Parameters for diamondback moth
#' dev_params()
#'
dev_params <- function(species = "dbm"){

  if(species == "dbm"){

    dev_params <- data.frame(
      egg       = c(0.0003592,   1.754,    35.08,     4.347),
      instar1_2 = c(0.00036,    -2.9122,   32.4565,  60.5092),
      instar3   = c(0.0009688,   0.7893,   32.04,    14.53),
      instar4   = c(0.000674,   -1.22561,  32.04873, 15.27334),
      prepupa   = c(0.00181533,  3.963006, 33.04467,  4.317786),
      pupa      = c(0.000396921, 2.417172, 32.44556, 11.99131)
      ) %>% t()

    colnames(dev_params) <- c("a", "Tmin", "Tmax", "m")

    return(dev_params)

  } else {stop("Invalid species: ", species)}
}

# NOTES:
# Unsure yet whether to export these parameters or just use them only internally.
