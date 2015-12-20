globalVariables("q.colors")
#' Distance between points
#' \code{Distance} A Euclidean distance between points
#'
#' @param e1x The x coordinate of a point.
#' @param e1y The y coordinate of a point.
#' @param e2x The x coordinate of another point.
#' @param e2y The y coordinate of another point.
#' @return The distance.
Distance <- function(e1x, e1y, e2x, e2y) {
  #
  sqrt(SquaredDistance(e1x, e1y, e2x, e2y))
}
#
#' Squared distance between points
#' \code{SquaredDistance} Squared Euclidean distance between points
#'
#' @param e1x The x coordinate of a point.
#' @param e1y The y coordinate of a point.
#' @param e2x The x coordinate of another point.
#' @param e2y The y coordinate of another point.
#' @return The squared distance.
SquaredDistance <- function(e1x, e1y, e2x, e2y) {
  (e2y - e1y)^2 + (e2x - e1x)^2
}

#' Distance to a line segment
#' \code{DistanceToLineSegment} Computes the distance between a point and the nearest point on a line segment
#'
#' @param px The x coordinate of a point.
#' @param py The y coordinate of a point.
#' @param ax The x coordinate of one end of a line.
#' @param ay The y coordinate of one end of a line.
#' @param bx The x coordinate of the other end of a line.
#' @param by The y coordinate of the other end of a line.
#' @return The distance.
DistanceToLineSegment <- function(px, py, ax, ay, bx, by) {
  l2 <- SquaredDistance(ax, ay, bx, by)
  if (l2 == 0)
    return(sqrt(SquaredDistance(px, py, ax, ay)))
  t <- ((px - ax) * (bx - ax) + (py - ay) * (by - ay)) / l2
  if (t < 0)
    return(sqrt(SquaredDistance(px, py, ax, ay)))
  if (t > 1)
    return(sqrt(SquaredDistance(px, py, bx, by)))
  sqrt(SquaredDistance(px, py, ax + t * (bx - ax), ay + t * (by - ay)))
}

#' Distance to a line segment
#' \code{DistanceToBox} Computes the distance between a point and the nearest point on a box (0 if in the box)
#'
#' @param px The x coordinate of a point.
#' @param py The y coordinate of a point.
#' @param ax The x coordinate of the bottom-left of the box.
#' @param ay The y coordinate of the bottom-left of the box.
#' @param bx The x coordinate of the bottom-right of the box.
#' @param by The y coordinate of the bottom-right of the box.
#' @param cx The x coordinate of the top-right of the box.
#' @param cy The y coordinate of the top-right of the box.
#' @param dx The xcoordinates of the top-left of the box.
#' @param dy The y coordinate of the top-left of the box.
#' @return The squared distance.
DistanceToBox <- function(px, py, ax, ay, bx, by, cx, cy, dx, dy) {
  d.horizontal <- Distance(ax, ay, bx, by)
  d.vertical <- Distance(bx, by, cx, cy)
  d.p.ab <- DistanceToLineSegment(px, py, ax, ay, bx, by)
  d.p.bc <- DistanceToLineSegment(px, py, bx, by, cx, cy)
  d.p.cd <- DistanceToLineSegment(px, py, cx, cy, dx, dy)
  d.p.da <- DistanceToLineSegment(px, py, dx, dy, ax, ay)
 # print(c(d.horizontal, d.vertical,d.p.ab, d.p.bc, d.p.cd, d.p.da ))
  # checking to see if point is in the box
  if (d.p.bc <= d.horizontal & d.p.da <= d.horizontal & d.p.ab <= d.vertical & d.p.cd <= d.vertical)
    return(0)
  min(d.p.ab, d.p.bc, d.p.cd, d.p.da)
}

# #' Whether or not two boxes overlap
# #' \code{BoxOverlap} Checks to see if two boxes overlap
# #'
# #' @param ax1 The x coordinate of the bottom-left of the first box.
# #' @param ay1 The y coordinate of the bottom-left of the first box.
# #' @param bx1 The x coordinate of the bottom-right of the first box.
# #' @param by1 The y coordinate of the bottom-right of the first box.
# #' @param cx1 The x coordinate of the top-right of the first box.
# #' @param cy1 The y coordinate of the top-right of the first box.
# #' @param dx1 The x coordinate of the top-left of the first box.
# #' @param dy1 The y coordinate of the top-left of the first box.
# #' @param ax2 The x coordinate of the bottom-left of the second box.
# #' @param ay2 The y coordinate of the bottom-left of the second box.
# #' @param bx2 The x coordinate of the bottom-right of the second box.
# #' @param by2 The y coordinate of the bottom-right of the second box.
# #' @param cx2 The x coordinate of the top-right of the second box.
# #' @param cy2 The y coordinate of the top-right of the second box.
# #' @param dx2 The x coordinate of the top-left of the second box.
# #' @param dy2 The y coordinate of the top-left of the second box.
# #' @return true if the boxes overlap and false otherwise.
# BoxOverlap <- function(ax1, ay1, bx1, by1, cx1, cy1, dx1, dy1, ax2, ay2, bx2, by2, cx2, cy2, dx2, dy2) {
#   # Returns: boolean
#   #
# public boolean overlaps (rectangle r) {
#     return x < r.x + r.width && x + width > r.x && y < r.y + r.height && y + height > r.y;
# }
# #   if (DistanceToBox(ax1, ay1, ax2, ay2, bx2, by2, cx2, cy2, dx2, dy2) == 0)
# #     return(TRUE)
# #   if (DistanceToBox(bx1, by1, ax2, ay2, bx2, by2, cx2, cy2, dx2, dy2) == 0)
# #     return(TRUE)
# #   if (DistanceToBox(cx1, cy1, ax2, ay2, bx2, by2, cx2, cy2, dx2, dy2) == 0)
# #     return(TRUE)
# #   if (DistanceToBox(dx1, dy1, ax2, ay2, bx2, by2, cx2, cy2, dx2, dy2) == 0)
# #     return(TRUE)
# #   if (DistanceToBox(ax2, ay2, ax1, ay1, bx1, by1, cx1, cy1, dx1, dy1) == 0)
# #     return(TRUE)
# #   if (DistanceToBox(bx2, by2, ax1, ay1, bx1, by1, cx1, cy1, dx1, dy1) == 0)
# #     return(TRUE)
# #   if (DistanceToBox(cx2, cy2, ax1, ay1, bx1, by1, cx1, cy1, dx1, dy1) == 0)
# #     return(TRUE)
# #   if (DistanceToBox(dx2, dy2, ax1, ay1, bx1, by1, cx1, cy1, dx1, dy1) == 0)
# #     return(TRUE)
# #   FALSE
# }

#' \code{rectangleOverlap} Checks to see if two recangles.
#'
#' @param a The first rectangle.
#' @param b The second recangle.
rectangleOverlap <- function(a, b)
{
    a$x < b$x + b$width &
        a$x + a$width > b$x &
        a$y < b$y + b$height &
        a$y + a$height > b$y;
}

#' \code{rectangle} The coordinates of a rectangle: bottom-left corner, width and height.
#'
#' @param x x coordinate of the middle of the box.
#' @param y y coordinate of the middle of the box.
#' @param w width of the box in the x-scale.
#' @param h height  of the box in the y-scale.
#' @return A list containing the coordinates of a box, consisting
#' of the bottom left coordinates, width and height.
rectangle = function(x, y, w, h)
{
  result <- list(x = x - w / 2, y = y - h / 2, width = w, height = h)
  class(result) <- "rectangle"
  result
}


# print.rectangle <- function(r)
# {
#     cat(paste("x:", r$x, " y:", r$y, "width", r$width, "height", r$height, "\n"))
# }

#' Box coordinates
#' \code{BoxOverlap} Computes the positions of the corner of a box.
#'
#' @param x x coordinate of the middle of the box.
#' @param y y coordinate of the middle of the box.
#' @param w width of the box in the x-scale.
#' @param h height  of the box in the y-scale.
#' @return A vector containing the coordinates of a box.
BoxCoordinates = function(x, y, w, h) {
  p1 <- x - w / 2
  p2 <- y - h / 2
  p3 = x + w / 2
  p4 = y + h / 2
  c(p1, p2, p3, p2, p3, p4, p1, p4)
}

# #' Box center
# #' \code{boxCenter} Computes the coordinates of the center of a box based upon the coordinates of the corner.
# #'
# #' @param ax The x coordinate of the bottom-left of the box.
# #' @param ay The y coordinate of the bottom-left of the box.
# #' @param bx The x coordinate of the bottom-right of the box.
# #' @param by The y coordinate of the bottom-right of the box.
# #' @param cx The x coordinate of the top-right of the box.
# #' @param cy The y coordinate of the top-right of the box.
# #' @param dx The x coordinate of the top-left of the box.
# #' @param dy The y coordinate of the top-left of the box.
# #' @return vector containing the x,y coordinates of the center.
# boxCenter = function(ax, ay, bx, by, cx, cy, dx, dy) {
#   c((ax + bx) / 2, (ay + dy) / 2)
# }

#' \code{rectangleOverlapIndex} Returns the index of the first overlapping rectangle encountered.
#'
#' @param rectangles A list of rectangles, where each rectangle is its coordinates.
#' @param this.rectangle The box to check to see if it overlaps with the rest.
#' @param this.rectangle.index The index of this.rectangle in the list of boxes.
#' @return -1 if no overlaps found..
rectangleOverlapIndex <- function(rectangles, this.rectangle, this.rectangle.index) {
    n <- length(rectangles)
    if (n == 0)
        return(-1)
    for (i in 1:n)
        if (i != this.rectangle.index)
        {
            if (rectangleOverlap(rectangles[[i]], this.rectangle))
                return(i)
        }
  -1
}

# #' Box Overlaps
# #' \code{OverlapIndex} Returns the index of the first overlapping box encountered.
# #'
# #' @param boxes A list of boxes, where each box is its coordinates.
# #' @param this.box The box to check to see if it overlaps with the rest.
# #' @param this.box.index The index of this.box in the list of boxes.
# #' @return -1 if no overlaps found..
# #' @export
# OverlapIndex <- function(boxes, this.box, this.box.index) {
#     if (length(boxes) == 0)
#         return(-1)
#     for (i in 1:length(boxes))
#         if (i != this.box.index)
#         {
#             box <- boxes[[i]]
#             if (BoxOverlap(box[1],box[2],box[3],box[4],box[5],box[6],box[7],box[8], this.box[1],this.box[2],this.box[3],this.box[4],this.box[5],this.box[6],this.box[7],this.box[8]))
#                 return(i)
#         }
#   -1
# }

#' ggplot xlim
#' \code{GetXlim}
#'
#' @param p A \code{\link[ggplot2]{ggplot}} plot.
#' @return A vector containing the minimum and and maximum value of x plotted.
GetXlim = function(p) {
  g = ggplot2::ggplot_build(p)
  g$panel$ranges[[1]]$x.range
}

#' ggplot ylim
#' \code{GetYlim}
#'
#' @param p A \code{\link[ggplot2]{ggplot}} plot.
#' @return A vector containing the minimum and and maximum value of y  plotted.
GetYlim <- function(p) {
  g <- ggplot2::ggplot_build(p)
  g$panel$ranges[[1]]$y.range
}

#' Find better cootrdinates for text labels
#' \code{GetYlim} Identifies a better set of coordinates to place labels in a labeled ggplot scatterplot.
#'
#' @param p A \code{\link[ggplot2]{ggplot}} plot.
#' @param do.nothing If TRUE, the algorithm does nothing, other than return values.
#' @param fixed.aspect If TRUE, forces the x and y dimensions to be on the same scale.
#' @param tstep The angle (theta) step size as the algorithm spirals out.
#' @param rstep The radius step size (in standard deviations) as the algorithm spirals out.
#' @param overlap.fudge Determines the amount of space required between labels. A value of 1 corresponds to a best guess of
#' no overlap. The guess can be wrong, so the plot can be improved by modifying this value, which has a muliplier effect.
#' @return dimensions Width and height of the text to be plotted in terms of the scale of x and y.
ReducePointAndLabelOverlap <- function (p, label.size, do.nothing = FALSE, fixed.aspect = FALSE, tstep = 0.1, rstep = 0.1, overlap.fudge = 1){
    # Inspired by  wordlayout {wordcloud}
    #
    # getting the coordinates
    labels <- p$data$labels
    label.coords <- point.coords <- p$data[,1:2]
    col.labels <- colnames(label.coords)
    n <- length(labels)
    # determining scale of the points and text relative to plot coordinates (dodgy hack)
    #print( par()$pin)
    #  print( p$data)
    #  print(p$panel$ranges[[1]])
    #  plot(p)
    #  stop("dog")
    #xlim <-
    rng.x <- diff(GetXlim(p)) * 1.1 #Adjustment to deal with space left outside of convex hull of points.
#      print("rng.x")
#      print(rng.x)
    rng.y <- diff(GetYlim(p)) * 1.1
#      print("rng.y")
#      print(rng.y)
    width.plotting.region <- par()$pin[1] * 25.4 * .8 # Hack due to inaccuracy of pin
#      print("width.plotting.region")
#      print(width.plotting.region)
    height.plotting.region <- par()$pin[2] * 25.4 * .96
#      print("height.plotting.region")
#      print(height.plotting.region)


    x.p.mm <- rng.x / width.plotting.region
    y.p.mm <- rng.y / height.plotting.region
#      print("x.p.mm")
#      print(x.p.mm)
#      print("y.p.mm")
#      print(y.p.mm)
#      print(fixed.aspect)
     if (fixed.aspect)
        x.p.mm <- y.p.mm <- max(x.p.mm, y.p.mm)
#      print("x.p.mm")
#      print(x.p.mm)
#      print("y.p.mm")
#      print(y.p.mm)
#
    inches.to.mm.fudge  <- label.size * 1.384615 * 1.8 / 1.95 / overlap.fudge
    character.height.mm <-  strheight("Jj", "inches") * inches.to.mm.fudge
    #line.height.y.scale <- character.height.mm * y.p.mm * 1.02 # Adjustment for gaps between lines
    smallish.size <- character.height.mm * x.p.mm
    widths <- strwidth(labels, "inches") * inches.to.mm.fudge * x.p.mm# / initial.x.scale
    n.lines <- 1 + stringr::str_count(labels, "\n")
    heights <- strheight(labels, "inches") * inches.to.mm.fudge * y.p.mm
    dimensions <- matrix(c(widths,heights), n, 2, dimnames = list(labels, c("width", "height")))
    # print(dimensions)
    sdx <- sd(label.coords[,1], na.rm = TRUE)
    sdy <- sd(label.coords[,2], na.rm = TRUE)
    if (sdx == 0)
    sdx <- 1
    if (sdy == 0)
    sdy <- 1
    # computing position of the points.
    # preventing overlap of points
#     for (i in 2:n)
#         for (prev.i in 1:(i - 1))
#             if (sum(point.coords[i,] == point.coords[prev.i,]) == 2)
#             {
#                 w <- widths[i]
#                 h <- heights[i]
#                 radian <- 2 * pi * (n + i) / (2 * n)
#                 point.coords[i, 1] <- point.coords[i, 1] + w / 4 * cos(radian)
#                 point.coords[i, 2] <- point.coords[i, 2] + h / 4 * sin(radian)
#             }
    # getting the coordinates for the labels, with the initial labels positioned
    # above the points if they are single lines of text, and in the "middle" otherwise, trying
    # to prevent the point being within a letter.
    offset <- rep(0, n)
#    offset[n.lines %% 2 == 1] <- 0.5
    offset[n.lines  == 1] <- 1
    offset[n.lines  == 2] <- 0.1
    offset[n.lines  == 3] <- 0.23
    label.coords[, 2] <- point.coords[, 2] + heights * offset
    # Computing the coordinates of the boxes at their initial positions (after being moved off the point)
    boxes <- vector("list", n)
    for (i in 1:n)
        boxes[[i]] <- rectangle(label.coords[i,1], label.coords[i,2],
          widths[i], heights[i])
    names(boxes) <- labels
    # print(boxes)
#
    # Moving labels outwards in a spiral until they no longer overlap.
    #
    #box.coordinates <- vector("list", n)
    thetas <- 2 * pi * (1:n) / n
    if (!do.nothing)
    {
        for (i in 1:n)
        {
            theta <- thetas[i]
            x1 <- label.coords[i, 1]
            y1 <- label.coords[i, 2]
            x0 <- point.coords[i, 1]
            y0 <- point.coords[i, 2]
            r <- 0
            label <- labels[i]
            w <- widths[i]
            h <- heights[i]
            overlapped <- TRUE
            new.position.counter <- 0
            while (overlapped)
            {
                this.box <- rectangle(x1, y1, w, h)
                #this.box <- BoxCoordinates(x1, y1, w, h)
                # print(paste("this.box", i))

                #print(boxes[[i]])
                #stop("dog")
                overlaps.with <- rectangleOverlapIndex(boxes, this.box, i)
                if (overlaps.with == -1)
                {
                    boxes[[i]] <- this.box
                    label.coords[i, ] <-  c(x1, y1)
                    overlapped <- FALSE
#                     print("no overlap")
#                     print(this.box)
                }
                else
                {
                    #
                    # print(paste("Moving", i, " due to overlap with ", overlaps.with))
                    # Trying to position the label on a different side of the point.
                    new.position.counter <- new.position.counter + 1
                    if (new.position.counter == 1)
                    {   # Trying to put the label lower down
                        y1 <- y0 - h * offset[i]
                    }
                    else if (new.position.counter == 2)
                    {    # trying to put the point to the left
                         x1 <- x0 - w / 2## - (smallish.size + widths[i])
                         y1 <- y0# - h * 1.2
                    }
                    else if (new.position.counter == 3)
                    {# trying to put the point to the right
                         x1 <- x0 + w / 2## - (smallish.size + widths[i])
                         y1 <- y0# - h * 1.2
                    }
                    else
                    {
                        theta <- theta + tstep
                        r <- r + rstep * tstep / (2 * pi)
                        x1 <- x0 + sdx * r * cos(theta)
                        y1 <- y0 + sdy * r * sin(theta)
                    }
                }
            }
        }
    }
    # print(boxes)
    list(label.coords = as.data.frame(label.coords), dimensions = dimensions)
}


#' Find better cootrdinates for text labels
#' \code{LabeledScatterPlot} Scatterplot with Labeled Points.
#' @param coords The xy coordinates of the points.
#' @param group A factor indicating group membership for each point.
#' @param row.labels A vector of labels which will, if supplied, over-ride the rownames of coodinates.
#' @param col.labels A vector of labels which will, if supplied, over-ride the colnames of coodinates.
#' @param title Title for the plot.
#' @param legend.title Title for the legend (which only appears if group is not null).
#' @param fixed.aspect if true, forces the x and y dimensions to be on the same scale.
#' @param auto.tidy Move the labels around so that fewer overlap.
#' @param colors Colors that are cycled through where there is only one series, or, used to demarkate series where there are multiple series.
#' @param auto.color Automatically colors the points (if FALSE, the first color is used).
#' @param general.color The color to be used in axes and titles.
#' @param point.size Size of the dot representing the poing (i.e., the glyph).
#' @param label.size Font size of the text labels on the plot.
#' @param axis.title.size Font size of the axis titles.
#' @param axis.label.size Font size of the axis labels.
#' @param tstep The angle (theta) step size as the algorithm spirals out.
#' @param rstep The radius step size (in standard deviations) as the algorithm spirals out.
#' @param title.size Font size of the chart title.
#' @param overlap.fudge Determines the amount of space required between labels. A value of 1 corresponds to a best guess of
#' no overlap. The guess can be wrong, so the plot can be improved by modifying this value, which has a muliplier effect.
#' @param space.substitute Spaces in labels of points on plots are substituted with whatever is supplied.
#' By default, a return character is used(i.e., \link{"\n"}). To replace with a period, use \link{"\\."}.
#' @param ... Additional arguments.
#'# @param object An object to be plotted.
#'# @param row.description A title for the rows.
#'# @param column.description A title for the columns.
#' @return p A \code{\link[ggplot2]{ggplot}} plot.
#'
# # MDS - square
# data(breakfastDissimilarities)
#     type = "interval", eps = 1e-12, itmax = 100000)
# LabeledScatterPlot(mdsInterval, title = "Interval-scale MDS of Breakfast Dissimilarities")
#
# data(colaPerceptions)
# LabeledScatterPlot(colaPerceptions[,c(7,8)], title = "Scatterplot of perceptions data",
#                           auto.tidy = TRUE, auto.color = 5, fixed.aspect = TRUE)
#' @export
LabeledScatterPlot <- function(coords, ...) UseMethod("LabeledScatterPlot")

#' @describeIn LabeledScatterPlot  Default labeled scatterplot
#' @export
LabeledScatterPlot.default = function(coords,  group = NULL, row.labels = NULL, col.labels = NULL,
                                      title = "",
                                      legend.title = "",
                                      space.substitute = "\n",
                                      fixed.aspect = TRUE,
                                      auto.tidy = TRUE,
                                      colors = q.colors,
                                      auto.color = TRUE,
                                      general.color = "gray28",
                                      point.size = 2,
                                      label.size = 10,
                                      legend.size = 10,
                                      axis.title.size = 12,
                                      axis.label.size = 10,
                                      title.size = 12,
                                      tstep = 0.1,
                                      rstep = 0.1,
                                      overlap.fudge = 1,
                                      ...) {
  # Replacing spaces with ~ to work around bugs in ggplot
  dimnames(coords)[[1]] <- gsub(" ", space.substitute, dimnames(coords)[[1]])
  # identifying the labels
  if (is.null(row.labels))
  {
    row.labels <- rownames(coords)
    #row.labels <-  gsub(" ", space.substitute, row.labels)
  }
  n <- length(row.labels)
  if (is.null(col.labels)) {
    col.labels <- colnames(coords)
    if(is.null(col.labels)) {
        col.labels <- c("Dimension 1", "Dimension 2")
    }
  }
  xlab <- "col1" #Due to bugs in ggplot, the true column names can only be added at the end.
  ylab <- "col2"
  dimnames(coords)[[2]] <- c(xlab, ylab)
    # Setting the colors
    #
    has.groups <- !is.null(group)
    if (!has.groups)
    {
        if (auto.color)
        {
            group <- array(1:length(colors), n)
        }
        else
        {
            group = rep(1, n)
        }
    }
    group <- factor(group)
  #
  # Setting limits to the axes so that they can easily accomodate the text
  #
  point.coords <- as.data.frame(coords)
  point.coords$labels <- row.labels
  # initial plot to get the coordinates
   p <- ggplot2::ggplot(point.coords, ggplot2::aes_string(x = "col1",
                                                          y = "col2", label = "labels"))
#   p <- ggplot2::ggplot(point.coords, ggplot2::aes_string(x = col.labels[1],
#                                                          y = col.labels[2], label = "labels"))
  # p <- p + ggplot2::labs(x = col.labels[1], y = "big dog", label = "labels")
  font.size.hack <- label.size * 25.4 / 72.77 # Converting from points to mm, only for geom_text
  p <- p + ggplot2::geom_point()
  if (fixed.aspect)
    p <- p + ggplot2::coord_fixed(ratio = 1)#, xlim = NULL, ylim = NULL, wise = NULL)
  #
  # moving points and labels to avoid overlap
  #
  new.coords <- ReducePointAndLabelOverlap(p, label.size, !auto.tidy, fixed.aspect = fixed.aspect, overlap.fudge = overlap.fudge)
  label.dimensions <- new.coords$dimensions
  label.coords <- new.coords$label.coords
  point.coords$labels <- label.coords$labels <- row.labels
  point.coords$group <- label.coords$group <- group
  smallest.x <- min(point.coords[,1], label.coords[,1] - label.dimensions[,1] / 2)
  biggest.x <- max(point.coords[,1], label.coords[,1] + label.dimensions[,1] / 2)
  smallest.y <- min(point.coords[,2], label.coords[,2] - label.dimensions[,2] / 2)
  biggest.y <- max(point.coords[,2], label.coords[,2] + label.dimensions[,2] / 2)

#   # Replacing spaces with ~ to work around bugs in ggplot
#   dimnames(label.coords)[[1]] <- gsub("\\.", " ", dimnames(label.coords)[[1]])
  #
  # creating the plot a second time
  #
  p <- ggplot2::ggplot(point.coords, ggplot2::aes_string(x = xlab, y = ylab))#, colour = "labels")) #+  scale_fill_manual(values=c("#F8766D", "#00BA38"))
  p <- p + ggplot2::theme_bw()
  p <- p + ggplot2::geom_point(size = point.size, ggplot2::aes(colour = group))
  p <- p + ggplot2::geom_text(data = label.coords,
                              ggplot2::aes_string(x = "col1", y = "col2",
          label = "labels", group = "group", colour = "group",
          lineheight = 1.02),
          size = font.size.hack, show_guide  = F )
  p <- p + ggplot2::labs(title = title, x = col.labels[1], y = col.labels[2])#, label = "labels")
#  p <- p + ggplot2::labs(title = title, x = col.labels[1], y = col.labels[2])#, label = "labels")
  p <- p + ggplot2::xlim(smallest.x, biggest.x) + ggplot2::ylim(smallest.y, biggest.y)
  p <- p + ggplot2::scale_colour_manual(values = colors, name = legend.title)
  if (fixed.aspect)
    p = p + ggplot2::coord_fixed(ratio = 1)#, xlim = NULL, ylim = NULL, wise = NULL)
  if (has.groups) {
    #p = p + scale_colour_manual(values = qColors, name = legend.title)
    p = p + ggplot2::theme(legend.text = ggplot2::element_text(colour = general.color, size = legend.size))
  } else {
    p <- p + ggplot2::theme(legend.position = "none")
  }
  p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(colour = general.color, size = axis.label.size))
  p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = general.color, size = axis.label.size))
  p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(colour = general.color, size = axis.title.size))
  p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(colour = general.color, size = axis.title.size))
  p <- p + ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.size, face = "bold"))
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(size = title.size, face="bold", vjust = 1.2))
  #
  # connecting points to text using lines
  #
  for (i in 1:n) {
    x <- coords[i, 1]
    y <- coords[i, 2]
    x1 <- label.coords[i, 1]
    y1 <- label.coords[i, 2]
    w <- label.dimensions[i, 1]
    h <- label.dimensions[i, 2]
    cs <- BoxCoordinates(x1, y1, w, h)
    abs.slope <- function(x1, y1, x2, y2) {
      abs((y1 - y2) / (x1 - x2))
    }
    if (DistanceToBox(x, y, cs[1], cs[2], cs[3], cs[4], cs[5], cs[6], cs[7], cs[8]) > h) {
      line.color = colors[group[i]]
      if (abs.slope(x, y, x1, y1) < 0.5) {
        if (x > x1) { # point to the righy of label
          p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[3], y = y, yend = cs[2] + h / 2), colour = line.color, size = .5)#, size = 3)
        } else  { # point to the left of label
          p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1], y = y, yend = cs[2] + h / 2), colour = line.color, size = .5)#, colour = transparent.colors[i], size = 3)
        }
      } else {
        if (y > y1) { # point above the label
          p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1] + w / 2, y = y, yend=cs[8]), colour = line.color, size = .5)##, colour = transparent.colors[i], size = 3)
        } else  { # point below the label
          p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1] + w / 2, y = y, yend=cs[2]), colour = line.color, size = .5)##, colour = transparent.colors[i], size = 3)
        }
      }
    }
  }
  p
}


#'  @describeIn LabeledScatterPlot  Labeled scatterplot of smacof object
#'  @export
LabeledScatterPlot.smacof = function(object, ...)
{
    LabeledScatterPlot.default(object$conf, fixed.aspect = TRUE, ...)
}

#'  @describeIn LabeledScatterPlot  Labeled scatterplot of smacofB object
#'  @export
LabeledScatterPlot.smacofB = function(object, ...)
{
    LabeledScatterPlot.default(object$conf, fixed.aspect = TRUE, ...)
}


#'  @describeIn LabeledScatterPlot  Labeled scatterplot of smacofR object
#'  @export
LabeledScatterPlot.smacofR = function(object, ...) # # row.description = "Rows", column.description = "Columns",  ...) {
{
    coords = rbind(object$conf.row, object$conf.col)
    group = c(rep(row.description, length(object$spp.row)),rep(column.description, length(object$spp.col)))
    LabeledScatterPlot.default(coords, fixed.aspect = TRUE, group = group, ...)
}


