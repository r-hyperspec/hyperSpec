#' @title Mark groups in [stats::hclust()] dendrograms
#'
#' @description
#' Groups are marked by colored rectangles as well as by their levels
#'
#' The dendrogram should be plotted separately, see the example.
#'
#' @param dendrogram the dendrogram
#' @param groups factor giving the the groups to mark
#' @param col vector with colors for each group
#' @param pos.marker top of the marker rectangle
#' @param height height of the marker rectangle
#' @param pos.text position of the text label
#' @param border see [graphics::text()]
#' @param text.col color (vector) giving the color for the text markers
#' @param label side label see example
#' @param label.right should the side labels be at the right side?
#' @param ... handed to [graphics::rect()] and [graphics::text()]
#'
#' @author Claudia Beleites
#'
#' @concept plotting
#' @concept plotting tools
#'
#' @export
#' @rdname mark_groups_in_dendrogram
#' @examples
#'
#' dend <- hclust(dist_pearson(laser[[]]))
#' par(xpd = TRUE, mar = c(5.1, 4, 4, 3)) # allows plotting into the margin
#' plot(dend, hang = -1, labels = FALSE)
#'
#' # mark clusters
#' clusters <- as.factor(cutree(dend, k = 4))
#' levels(clusters) <- LETTERS[1:4]
#' mark_groups_in_dendrogram(dend, clusters, label = "cluster")
#'
#' # mark independent factor
#' mark_groups_in_dendrogram(dend, as.factor(laser[, , 405.36] > 11000),
#'   pos.marker = -0.02, pos.text = -0.03
#' )
#'
#' # mark continuous variable: convert it to a factor and omit labels
#' mark_groups_in_dendrogram(dend, cut(laser[[, , 405.36]], 100),
#'   palette_alois(100), pos.marker = -.015, text.col = NA,
#'   label = expression(I[lambda == 405.36 ~ nm]), label.right = FALSE
#' )
#' @importFrom utils head tail
mark_groups_in_dendrogram <- function(dendrogram, groups,
                                      col = seq_along(unique(groups)),
                                      pos.marker = 0,
                                      height = 0.025 * max(dendrogram$height),
                                      pos.text = -2.5 * height,
                                      border = NA,
                                      text.col = "black",
                                      label,
                                      label.right = TRUE,
                                      ...) {
  if (!is.factor(groups)) {
    groups <- as.factor(groups)
  }

  groups.x <- groups[dendrogram$order] # clusters in order on x axis

  rle.groups <- rle(as.integer(groups.x)) # run-length encoding gives borders

  end <- cumsum(rle.groups$lengths) + 0.5
  start <- c(0.5, (head(end, -1)))
  text <- (start + end) / 2

  text.col <- rep(text.col, length.out = length(text))

  for (g in seq_along(rle.groups$lengths)) {
    rect(
      xleft = start[g], ybottom = pos.marker - height,
      xright = end[g], ytop = pos.marker,
      col = col[rle.groups$values[g]], border = border, ...
    )
    if (!is.na(text.col[g])) {
      text(
        x = text[g], y = pos.text,
        levels(groups)[rle.groups$values[g]],
        col = text.col[rle.groups$values[g]], ...
      )
    }
  }

  if (!missing(label)) {
    text(
      x = label.right * tail(end, 1) * 1.01, y = pos.marker - height / 2,
      label, adj = c(1 - label.right, .5)
    )
  }
}
