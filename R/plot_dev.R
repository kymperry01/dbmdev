#' Plot development predictions for the diamondback moth
#'
#' @param df A \code{data.frame} with developmental predictions output by
#' \code{predict_dev}.
#' @param what What output to plot, either "increments", "stages",
#' or "gens" (character).
#'
#' @param display Optionally display the plot in the console, TRUE or
#' FALSE (logical).
#'
#' @return A \code{ggplot} object
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' library(RColorBrewer)
#'
#' ## Note: Works properly for 1 gen only. Draft function.
#'
#' # Generate some sample temperatures at a given location
#' d1 <- daily(days = 200, start_date = "2023-09-01")
#' h1 <- hourly(d1)
#'
#' # Predict forward 2 generations from the "egg" stage
#' out1 <- predict_dev(h1, start_date = "2023-09-02", gens = 1)
#'
#' # Plot
#' plot_dev(out1)
#'
#' ## End
plot_dev <- function(
    df#,
    #what = "stages" Todo: implement choice
    ) {

  # control maximum size of outputs to plot
  # add

  ybreaks <- c(df$start_dev[1], df$complete_dev)

  df %>%
    ggplot(aes(
      x = 0.5,
      xend = 0.5,
      y = start_dev,
      yend = complete_dev,
      colour = stage
    )) +
    geom_segment(
      # lineend = "butt",
      linejoin = "round",
      linewidth = 3
    ) +
    geom_point(
      pch = 21,
      fill = "black",
      colour = "black",
      size = 1
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      aspect.ratio = 0.2,
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_colour_brewer(palette = "Set2") +
    scale_y_datetime(
      breaks = ybreaks,
      date_labels = "%d-%b"
    ) +
    labs(x = NULL,
         y = NULL,
         colour = NULL)
}


