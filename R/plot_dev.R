#' Plot development predictions for the diamondback moth
#'
#' @param x Developmental predictions as output by \code{predict_dev}.
#' @param what What output to plot, either "increments", "stages",
#' or "gens" (character).
#' @param ... Passed to [ggplot2::geom_point()] and [ggplot2::geom_segment()]
#' @param xlab,ylab Axis labels
#' @param date_fmt Format for printing x-axis labels. See [base::strptime()]
#'
#'
#' @return A \code{ggplot} object
#' @export
#'
#' @examples
#' library(dplyr)
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
#' @import ggplot2
plot_dev <- function(
    x, what = "stages", ..., xlab = NULL, ylab = NULL, date_fmt = "%d-%b"
) {

  what <- match.arg(what)
  what <- match.arg(what, names(x))
  df <- x[[what]]

  if (what == "stages") {
    p <- .plot_stages(df, ..., date_fmt = date_fmt)
  }
  p + labs(x = xlab, y = ylab)

}

.plot_stages <- function(df, ..., date_fmt) {

  dot_args <- list(...)
  ## For geom_point, set some defaults which can be overwritten easily
  valid_point_args <- c("alpha", "colour", "fill", "shape", "size", "stroke")
  def_point_args <- list(fill = "black", colour = "black", size = 1, shape = 21)
  point_args <- c(
    dot_args[names(dot_args) %in% valid_point_args],
    def_point_args[!names(def_point_args) %in% names(dot_args)]
  )
  ## Repeat for geom_segment
  valid_seg_args <- c("linetype", "linewidth", "lineend", "linejoin", "arrow")
  def_seg_args <- list(lineend = "round", linewidth = 3)
  seg_args <- c(
    dot_args[names(dot_args) %in% valid_seg_args],
    def_seg_args[!names(def_seg_args) %in% names(dot_args)]
  )
  point_args$y <- seg_args$y <- seg_args$yend <- 0.5

  xbreaks <- c(df$start_dev[1], df$complete_dev)
  ## Putting these here avoids the R CMD check, but also side-steps having to
  ## introduce rlang as a dependency & having to use !!sym("start_dev") etc
  start_dev <- complete_dev <- stage <- NULL
  ggplot(
    df, aes(x = start_dev, xend = complete_dev, colour = stage)
  ) +
    do.call("geom_segment", seg_args) +
    do.call("geom_point", point_args) +
    theme(
      aspect.ratio = 0.2, panel.grid = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    scale_x_datetime(breaks = xbreaks, date_labels = date_fmt)
}
