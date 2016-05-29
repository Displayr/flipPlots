#' Scatterplot with Labeled Points.
#'
#' \code{LabeledScatterPlot} Scatterplot with Labeled Points.
#' @rdname LabeledScatterPlot
#' @export LabeledScatterPlot
LabeledScatterPlot <- function(coords, ...) UseMethod("LabeledScatterPlot", coords)

#' Scatterplot with Labeled Points.
#'
#' \code{LabeledScatterPlot} Scatterplot with Labeled Points.
#' @param coords The xy coordinates of the points.
#' @param group A factor indicating group membership for each point.
#' @param row.labels A vector of labels which will, if supplied, over-ride the rownames of coodinates.
#' @param column.labels A vector of labels which will, if supplied, over-ride the colnames of coodinates.
#' @param title Title for the plot.
#' @param group.name Title for the legend (which only appears if group is not null).
#' @param fixed.aspect if true, forces the x and y dimensions to be on the same scale.
#' @param colors Colors that are cycled through where there is only one series, or, used to demarkate series where there are multiple series.
#' @param auto.color Automatically colors the points (if FALSE, the first color is used).
#' @param general.color The color to be used in axes and titles.
#' @param point.size Size of the dot representing the poing (i.e., the glyph).
#' @param label.font.size Font size of the text labels on the plot.
#' @param axis.title.font.size Font size of the axis titles.
#' @param axis.label.font.size Font size of the axis labels.
#' @param title.font.size Font size of the chart title.
#' @param legend.font.size Font size of the for the plot.
#' @param ... Other plotting arguments.
#' @return A \code{\link[ggplot2]{ggplot}} plot.
#' @rdname LabeledScatterPlot
#' @method LabeledScatterPlot default
#' @export LabeledScatterPlot
LabeledScatterPlot.default = function(coords,
                            group = NULL,
                            row.labels = NULL,
                            column.labels = NULL,
                            title = "",
                            group.name = "",
                            fixed.aspect = FALSE,
                            #auto.tidy = TRUE,
                            colors = QColors,
                            auto.color = TRUE,
                            general.color = "gray28",
                            point.size = 1,
                            label.font.size = 10,
                            legend.font.size = 10,
                            axis.title.font.size = 12,
                            axis.label.font.size = 10,
                            title.font.size = 12)
{
    # Extracting the labels
    if (is.null(row.labels))
        row.labels <- rownames(coords)
    #print(class(coords))
#    space.substitute = " "
#    rownames(coords) <- row.labels <- gsub(" ", space.substitute, row.labels)
    n <- length(row.labels)
    if (is.null(column.labels))
        column.labels <- colnames(coords)
    if(is.null(column.labels))
        column.labels <- c("Dimension 1", "Dimension 2")
    xlab <- "col1" #Due to bugs in ggplot, the true column names can only be added at the end.
    ylab <- "col2"
    dimnames(coords)[[2]] <- c(xlab, ylab)
    # Setting the colors
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
    # point.coords <- as.data.frame(coords)
    # point.coords$labels <- row.labels
    # # initial plot to get the coordinates
    # p <- ggplot2::ggplot(point.coords, ggplot2::aes_string(x = "col1", y = "col2", label = "labels"))
    # font.size.hack <- label.font.size * 25.4 / 72.77 # Converting from points to mm, only for geom_text
    # p <- p + ggplot2::geom_point()
    # if (fixed.aspect)
    #     p <- p + ggplot2::coord_fixed(ratio = 1)#, xlim = NULL, ylim = NULL, wise = NULL)
    #
    # moving points and labels to avoid overlap
    #
    # new.coords <- ReducePointAndLabelOverlap(p, label.font.size, !auto.tidy,
    #     fixed.aspect = fixed.aspect, tstep = tstep,rstep = rstep, overlap.fudge = overlap.fudge)
    coords <- data.frame(coords)#$dimensions
#    label.coords <- coords#new.coords#$label.coords
    coords$labels <- row.labels
    coords$group <- group
    smallest.x <- min(coords[,1])#, coords[,1] - label.dimensions[,1] / 2)
    biggest.x <- max(coords[,1])#, coords[,1] + label.dimensions[,1] / 2)
    smallest.y <- min(coords[,2])#, coords[,2] - label.dimensions[,2] / 2)
    biggest.y <- max(coords[,2])#, coords[,2] + label.dimensions[,2] / 2)
    #
    p <- ggplot2::ggplot(coords, ggplot2::aes_string(x = xlab, y = ylab))#, colour = "labels")) #+  scale_fill_manual(values=c("#F8766D", "#00BA38"))
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::geom_point(size = point.size, ggplot2::aes(colour = group))
#    p <- p + geom_text_repel(aes(x = xlab, y = ylab, colour = group, label = label.coords))
    set.seed(1)
   # if (auto.tidy)
        p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = row.labels,  group = group, colour = group),
             box.padding = ggplot2::unit(0.35, "lines"),
            point.padding = ggplot2::unit(0.3, "lines"), size = label.font.size / 14 * 5)
#    else
 #       p <- p + ggplot2::geom_text(data = coords,
  #          ggplot2::aes_string(x = "col1", y = "col2",
   #         label = row.labels, group = "group", colour = "group",
    #        lineheight = 1.02),
     #       size = , size = label.font.size / 14 * 5, show.legend = FALSE)
        # p <- p + ggplot2::aes_string(x = "col1", y = "col2", label = "labels", group = "group", colour = "group",
        #  lineheight = 1.02),size = label.font.size, show.legend = F)

    #p <- p + ggplot2::geom_text(data = label.coords,
        # ggplot2::aes_string(x = "col1", y = "col2",
        #     label = "labels", group = "group", colour = "group",
        #     lineheight = 1.02),
        # size = font.size.hack, show.legend = F)
    p <- p + ggplot2::labs(title = title, x = column.labels[1], y = column.labels[2])#, label = "labels")
    p <- p + ggplot2::xlim(smallest.x, biggest.x) + ggplot2::ylim(smallest.y, biggest.y)
    p <- p + ggplot2::scale_colour_manual(values = colors, name = group.name)
    if (fixed.aspect)
      p <- p + ggplot2::coord_fixed(ratio = 1)#, xlim = NULL, ylim = NULL, wise = NULL)
    if (has.groups)
    {
        p <- p + ggplot2::theme(legend.text = ggplot2::element_text(colour = general.color, size = legend.font.size * 5 / 14)) +
            ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4)))
    }
    else
        p <- p + ggplot2::theme(legend.position = "none")
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(colour = general.color, size = axis.label.font.size))
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = general.color, size = axis.label.font.size))
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(colour = general.color, size = axis.title.font.size))
    p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(colour = general.color, size = axis.title.font.size))
    p <- p + ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.font.size, face = "bold"))
    p <- p + ggplot2::theme(plot.title = ggplot2::element_text(size = title.font.size, face="bold", vjust = 1.2))
    if (smallest.x < 0 & biggest.x > 0)
        p <- p + ggplot2::geom_vline(xintercept = 0, colour = general.color, linetype="dashed")
    if (smallest.y < 0 & biggest.y > 0)
        p <- p + ggplot2::geom_hline(yintercept = 0, colour = general.color, linetype="dashed")
    p <- p + ggplot2::theme(legend.key = ggplot2::element_rect(colour = "NA"))#, base_size = label.font.size)
    #
    # connecting points to text using lines
    #
    # for (i in 1:n) {
    #     x <- coords[i, 1]
    #     y <- coords[i, 2]
    #     x1 <- label.coords[i, 1]
    #     y1 <- label.coords[i, 2]
    #     w <- label.dimensions[i, 1]
    #     h <- label.dimensions[i, 2]
    #     cs <- BoxCoordinates(x1, y1, w, h)
    #     .absSlope <- function(x1, y1, x2, y2) abs((y1 - y2) / (x1 - x2))
    #     if (DistanceToBox(x, y, cs[1], cs[2], cs[3], cs[4], cs[5], cs[6], cs[7], cs[8]) > h) {
    #         line.color = colors[group[i]]
    #         if (.absSlope(x, y, x1, y1) < 0.5) {
    #             if (x > x1) { # point to the righy of label
    #                 p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[3], y = y, yend = cs[2] + h / 2), colour = line.color, size = .5)#, size = 3)
    #         } else  { # point to the left of label
    #           p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1], y = y, yend = cs[2] + h / 2), colour = line.color, size = .5)#, colour = transparent.colors[i], size = 3)
    #         }
    #       } else {
    #         if (y > y1) { # point above the label
    #           p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1] + w / 2, y = y, yend=cs[8]), colour = line.color, size = .5)##, colour = transparent.colors[i], size = 3)
    #         } else  { # point below the label
    #           p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1] + w / 2, y = y, yend=cs[2]), colour = line.color, size = .5)##, colour = transparent.colors[i], size = 3)
    #         }
    #       }
    #     }
    #   }
      p
}

#' @rdname LabeledScatterPlot
#' @method LabeledScatterPlot smacof
#' @export
LabeledScatterPlot.smacof = function(object, ...)
{
    LabeledScatterPlot.default(object$conf, fixed.aspect = TRUE, ...)
}

#' @rdname LabeledScatterPlot
#' @method LabeledScatterPlot smacofB
#' @export
LabeledScatterPlot.smacofB = function(object, ...)
{
    LabeledScatterPlot.default(object$conf, fixed.aspect = TRUE, ...)
}


#' @rdname LabeledScatterPlot
#' @method LabeledScatterPlot smacofR
#' @export
LabeledScatterPlot.smacofR = function(object, ...) # # row.description = "Rows", column.description = "Columns",  ...) {
{
    coords = rbind(object$conf.row, object$conf.col)
    group = c(rep(row.description, length(object$spp.row)),rep(column.description, length(object$spp.col)))
    LabeledScatterPlot.default(coords, fixed.aspect = TRUE, group = group, ...)
}


#'
# # MDS - square
# data(breakfastDissimilarities)
#     type = "interval", eps = 1e-12, itmax = 100000)
# LabeledScatterPlot(mdsInterval, title = "Interval-scale MDS of Breakfast Dissimilarities")
#
# data(colaPerceptions)
# LabeledScatterPlot(colaPerceptions[,c(7,8)], title = "Scatterplot of perceptions data",
#                           auto.tidy = TRUE, auto.color = 5, fixed.aspect = TRUE)

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
#' #
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
#'
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
#'
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
#'
#' #' \code{rectangleOverlap} Checks to see if two recangles.
#' #'
#' #' @param a The first rectangle.
#' #' @param b The second recangle.
#' rectangleOverlap <- function(a, b)
#' {
#'     a$x < b$x + b$width &
#'         a$x + a$width > b$x &
#'         a$y < b$y + b$height &
#'         a$y + a$height > b$y;
#' }
#'
#' #' \code{rectangle} The coordinates of a rectangle: bottom-left corner, width and height.
#' #'
#' #' @param x x coordinate of the middle of the box.
#' #' @param y y coordinate of the middle of the box.
#' #' @param w width of the box in the x-scale.
#' #' @param h height  of the box in the y-scale.
#' #' @return A list containing the coordinates of a box, consisting
#' #' of the bottom left coordinates, width and height.
#' rectangle = function(x, y, w, h)
#' {
#'   result <- list(x = x - w / 2, y = y - h / 2, width = w, height = h)
#'   class(result) <- "rectangle"
#'   result
#' }
#'
#'
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

#' #' \code{rectangleOverlapIndex} Returns the index of the first overlapping rectangle encountered.
#' #'
#' #' @param rectangles A list of rectangles, where each rectangle is its coordinates.
#' #' @param this.rectangle The box to check to see if it overlaps with the rest.
#' #' @param this.rectangle.index The index of this.rectangle in the list of boxes.
#' #' @return -1 if no overlaps found..
#' rectangleOverlapIndex <- function(rectangles, this.rectangle, this.rectangle.index) {
#'     n <- length(rectangles)
#'     if (n == 0)
#'         return(-1)
#'     for (i in 1:n)
#'         if (i != this.rectangle.index)
#'         {
#'             if (rectangleOverlap(rectangles[[i]], this.rectangle))
#'                 return(i)
#'         }
#'   -1
#' }
#'
#' #' ggplot xlim
#' #' \code{GetXlim}
#' #'
#' #' @param p A \code{\link[ggplot2]{ggplot}} plot.
#' #' @return A vector containing the minimum and and maximum value of x plotted.
#' GetXlim = function(p) {
#'   g = ggplot2::ggplot_build(p)
#'   g$panel$ranges[[1]]$x.range
#' }
#'
#' #' ggplot ylim
#' #' \code{GetYlim}
#' #'
#' #' @param p A \code{\link[ggplot2]{ggplot}} plot.
#' #' @return A vector containing the minimum and and maximum value of y  plotted.
#' GetYlim <- function(p) {
#'   g <- ggplot2::ggplot_build(p)
#'   g$panel$ranges[[1]]$y.range
#' }
#' #'
#' #' #' Find better cootrdinates for text labels
#' #' #' \code{GetYlim} Identifies a better set of coordinates to place labels in a labeled ggplot scatterplot.
#' #' #'
#' #' #' @param p A \code{\link[ggplot2]{ggplot}} plot.
#' #' #' @param do.nothing If TRUE, the algorithm does nothing, other than return values.
#' #' #' @param fixed.aspect If TRUE, forces the x and y dimensions to be on the same scale.
#' #' #' @param tstep The angle (theta) step size as the algorithm spirals out.
#' #' #' @param rstep The radius step size (in standard deviations) as the algorithm spirals out.
#' #' #' @param overlap.fudge Determines the amount of space required between labels. A value of 1 corresponds to a best guess of
#' #' #' no overlap. The guess can be wrong, so the plot can be improved by modifying this value, which has a muliplier effect.
#' #' #' @param plot.width The width of the plotting area (in inches).
#' #' #' @param plot.height The height of the plotting area (in inches).
#' #' #' @return dimensions Width and height of the text to be plotted in terms of the scale of x and y.
#' #' ReducePointAndLabelOverlap <- function (p, label.font.size,
#' #'                                         do.nothing,
#' #'                                         fixed.aspect,
#' #'                                         tstep,
#' #'                                         rstep,
#' #'                                         overlap.fudge)
#' #' {
#' #'     # Inspired by  wordlayout {wordcloud}
#' #'     # getting the coordinates
#' #'     labels <- p$data$labels
#' #'     label.coords <- point.coords <- p$data[,1:2]
#' #'     column.labels <- colnames(label.coords)
#' #'     n <- length(labels)
#' #'     # determining scale of the points and text relative to plot coordinates (dodgy hack)
#' #'     rng.x <- diff(GetXlim(p)) * 1.1 #Adjustment to deal with space left outside of convex hull of points.
#' #'     rng.y <- diff(GetYlim(p)) * 1.1
#' #'     width.plotting.region <- ifelse(exists("QOutputSizeWidth"), QOutputSizeWidth, dev.size("in")[1]) * 25.4 * .9 # Hack due to plotting margins
#' #'     height.plotting.region <- ifelse(exists("QOutputSizeHeight"), QOutputSizeHeight, dev.size("in")[2]) * 25.4 * .80
#' #'     x.p.mm <- rng.x / width.plotting.region
#' #'     y.p.mm <- rng.y / height.plotting.region
#' #'      if (fixed.aspect)
#' #'         x.p.mm <- y.p.mm <- max(x.p.mm, y.p.mm)
#' #'     inches.to.mm.fudge  <- label.font.size * 1.384615 * 1.8 / 1.95 / overlap.fudge
#' #'     character.height.mm <-  strheight("Jj", "inches") * inches.to.mm.fudge
#' #'     smallish.size <- character.height.mm * x.p.mm
#' #'     widths <- strwidth(labels, "inches") * inches.to.mm.fudge * x.p.mm# / initial.x.scale
#' #'     n.lines <- 1 + stringr::str_count(labels, "\n")
#' #'     heights <- strheight(labels, "inches") * inches.to.mm.fudge * y.p.mm
#' #'     dimensions <- matrix(c(widths,heights), n, 2, dimnames = list(labels, c("width", "height")))
#' #'     sdx <- sd(label.coords[,1], na.rm = TRUE)
#' #'     sdy <- sd(label.coords[,2], na.rm = TRUE)
#' #'     if (sdx == 0)
#' #'         sdx <- 1
#' #'     if (sdy == 0)
#' #'         sdy <- 1
#' #'     # computing position of the points.
#' #'     # getting the coordinates for the labels, with the initial labels positioned
#' #'     # above the points if they are single lines of text, and in the "middle" otherwise, trying
#' #'     # to prevent the point being within a letter.
#' #'     offset <- rep(0, n)
#' #'     offset[n.lines  == 1] <- 1
#' #'     offset[n.lines  == 2] <- 0.1
#' #'     offset[n.lines  == 3] <- 0.23
#' #'     label.coords[, 2] <- point.coords[, 2] + heights * offset
#' #'     # Computing the coordinates of the boxes at their initial positions (after being moved off the point)
#' #'     boxes <- vector("list", n)
#' #'     for (i in 1:n)
#' #'         boxes[[i]] <- rectangle(label.coords[i,1], label.coords[i,2],
#' #'           widths[i], heights[i])
#' #'     names(boxes) <- labels
#' #'     # Moving labels outwards in a spiral until they no longer overlap.
#' #'     thetas <- 2 * pi * (1:n) / n
#' #'     if (!do.nothing)
#' #'     {
#' #'         for (i in 1:n)
#' #'         {
#' #'             theta <- thetas[i]
#' #'             x1 <- label.coords[i, 1]
#' #'             y1 <- label.coords[i, 2]
#' #'             x0 <- point.coords[i, 1]
#' #'             y0 <- point.coords[i, 2]
#' #'             r <- 0
#' #'             label <- labels[i]
#' #'             w <- widths[i]
#' #'             h <- heights[i]
#' #'             overlapped <- TRUE
#' #'             new.position.counter <- 0
#' #'             iteration <- 0
#' #'             while (overlapped & iteration < 1000)
#' #'             {
#' #'                 iteration <- iteration + 1
#' #'                 this.box <- rectangle(x1, y1, w, h)
#' #'                 overlaps.with <- rectangleOverlapIndex(boxes, this.box, i)
#' #'                 if (overlaps.with == -1)
#' #'                 {
#' #'                     boxes[[i]] <- this.box
#' #'                     label.coords[i, ] <-  c(x1, y1)
#' #'                     overlapped <- FALSE
#' #'                 }
#' #'                 else
#' #'                 {
#' #'                     # Trying to position the label on a different side of the point.
#' #'                     new.position.counter <- new.position.counter + 1
#' #'                     if (new.position.counter == 1)
#' #'                     {   # Trying to put the label lower down
#' #'                         y1 <- y0 - h * offset[i]
#' #'                     }
#' #'                     else if (new.position.counter == 2)
#' #'                     {    # trying to put the point to the left
#' #'                          x1 <- x0 - w / 2## - (smallish.size + widths[i])
#' #'                          y1 <- y0# - h * 1.2
#' #'                     }
#' #'                     else if (new.position.counter == 3)
#' #'                     {# trying to put the point to the right
#' #'                          x1 <- x0 + w / 2## - (smallish.size + widths[i])
#' #'                          y1 <- y0# - h * 1.2
#' #'                     }
#' #'                     else
#' #'                     {
#' #'                         theta <- theta + tstep
#' #'                         r <- r + rstep * tstep / (2 * pi)
#' #'                         x1 <- x0 + sdx * r * cos(theta)
#' #'                         y1 <- y0 + sdy * r * sin(theta)
#' #'                     }
#' #'                 }
#' #'             }
#' #'         }
#' #'     }
#' #'     list(label.coords = as.data.frame(label.coords), dimensions = dimensions)
#' #' }
#' #'
#'
#' #' \code{LabeledScatterPlot} Scatterplot with Labeled Points.
#' #' @param coords The xy coordinates of the points.
#' #' @param group A factor indicating group membership for each point.
#' #' @param row.labels A vector of labels which will, if supplied, over-ride the rownames of coodinates.
#' #' @param column.labels A vector of labels which will, if supplied, over-ride the colnames of coodinates.
#' #' @param main Title for the plot.
#' #' @param group.name Title for the legend (which only appears if group is not null).
#' #' @param fixed.aspect if true, forces the x and y dimensions to be on the same scale.
#' #' @param auto.tidy Move the labels around so that fewer overlap.
#' #' @param colors Colors that are cycled through where there is only one series, or, used to demarkate series where there are multiple series.
#' #' @param auto.color Automatically colors the points (if FALSE, the first color is used).
#' #' @param general.color The color to be used in axes and titles.
#' #' @param point.size Size of the dot representing the poing (i.e., the glyph).
#' #' @param label.font.size Font size of the text labels on the plot.
#' #' @param axis.title.font.size Font size of the axis titles.
#' #' @param axis.label.font.size Font size of the axis labels.
#' #' @param tstep The angle (theta) step size as the algorithm spirals out.
#' #' @param rstep The radius step size (in standard deviations) as the algorithm spirals out.
#' #' @param title.font.size Font size of the chart title.
#' #' @param overlap.fudge Determines the amount of space required between labels. A value of 1 corresponds to a best guess of
#' #' no overlap. The guess can be wrong, so the plot can be improved by modifying this value, which has a muliplier effect.
#' #' @param space.substitute Spaces in labels of points on plots are substituted with whatever is supplied.
#' #' By default, a return character is used(i.e., \code{"\n"}). To replace with a period, use \code{"\\."}.
#' #' @return p A \code{\link[ggplot2]{ggplot}} plot.
#' #' @export
#' LabeledScatterPlot = function(coords,  group = NULL, row.labels = NULL, column.labels = NULL,
#'                                       main = "",
#'                                       group.name = "",
#'                                       space.substitute = "\n",
#'                                       fixed.aspect = FALSE,
#'                                       auto.tidy = TRUE,
#'                                       colors = QColors,
#'                                       auto.color = TRUE,
#'                                       general.color = "gray28",
#'                                       point.size = 1,
#'                                       label.font.size = 10,
#'                                       legend.size = 10,
#'                                       axis.title.font.size = 12,
#'                                       axis.label.font.size = 10,
#'                                       main.size = 12,
#'                                       tstep = 0.1,
#'                                       rstep = 0.1,
#'                                       overlap.fudge = 1)
#' {
#'     # Extracting the labels
#'     if (is.null(row.labels))
#'         row.labels <- rownames(coords)
#'     rownames(coords) <- row.labels <- gsub(" ", space.substitute, row.labels)
#'     n <- length(row.labels)
#'     if (is.null(column.labels))
#'         column.labels <- colnames(coords)
#'     if(is.null(column.labels))
#'         column.labels <- c("Dimension 1", "Dimension 2")
#'     xlab <- "col1" #Due to bugs in ggplot, the true column names can only be added at the end.
#'     ylab <- "col2"
#'     dimnames(coords)[[2]] <- c(xlab, ylab)
#'     # Setting the colors
#'     has.groups <- !is.null(group)
#'     if (!has.groups)
#'     {
#'         if (auto.color)
#'         {
#'             group <- array(1:length(colors), n)
#'         }
#'         else
#'         {
#'             group = rep(1, n)
#'         }
#'     }
#'     group <- factor(group)
#'     #
#'     # Setting limits to the axes so that they can easily accomodate the text
#'     #
#'     point.coords <- as.data.frame(coords)
#'     point.coords$labels <- row.labels
#'     # initial plot to get the coordinates
#'     p <- ggplot2::ggplot(point.coords, ggplot2::aes_string(x = "col1", y = "col2", label = "labels"))
#'     font.size.hack <- label.font.size * 25.4 / 72.77 # Converting from points to mm, only for geom_text
#'     p <- p + ggplot2::geom_point()
#'     if (fixed.aspect)
#'         p <- p + ggplot2::coord_fixed(ratio = 1)#, xlim = NULL, ylim = NULL, wise = NULL)
#'     #
#'     # moving points and labels to avoid overlap
#'     #
#'     new.coords <- ReducePointAndLabelOverlap(p, label.font.size, !auto.tidy,
#'         fixed.aspect = fixed.aspect, tstep = tstep,rstep = rstep, overlap.fudge = overlap.fudge)
#'     label.dimensions <- new.coords$dimensions
#'     label.coords <- new.coords$label.coords
#'     point.coords$labels <- label.coords$labels <- row.labels
#'     point.coords$group <- label.coords$group <- group
#'     smallest.x <- min(point.coords[,1], label.coords[,1] - label.dimensions[,1] / 2)
#'     biggest.x <- max(point.coords[,1], label.coords[,1] + label.dimensions[,1] / 2)
#'     smallest.y <- min(point.coords[,2], label.coords[,2] - label.dimensions[,2] / 2)
#'     biggest.y <- max(point.coords[,2], label.coords[,2] + label.dimensions[,2] / 2)
#'     #
#'     # creating the plot a second time
#'     #
#'     p <- ggplot2::ggplot(point.coords, ggplot2::aes_string(x = xlab, y = ylab))#, colour = "labels")) #+  scale_fill_manual(values=c("#F8766D", "#00BA38"))
#'     p <- p + ggplot2::theme_bw()
#'     p <- p + ggplot2::geom_point(size = point.size, ggplot2::aes(colour = group))
#'     p <- p + ggplot2::geom_text(data = label.coords,
#'         ggplot2::aes_string(x = "col1", y = "col2",
#'             label = "labels", group = "group", colour = "group",
#'             lineheight = 1.02),
#'         size = font.size.hack, show.legend = F)
#'     p <- p + ggplot2::labs(title = main, x = column.labels[1], y = column.labels[2])#, label = "labels")
#'     p <- p + ggplot2::xlim(smallest.x, biggest.x) + ggplot2::ylim(smallest.y, biggest.y)
#'     p <- p + ggplot2::scale_colour_manual(values = colors, name = group.name)
#'     if (fixed.aspect)
#'       p <- p + ggplot2::coord_fixed(ratio = 1)#, xlim = NULL, ylim = NULL, wise = NULL)
#'     if (has.groups)
#'     {
#'         p <- p + ggplot2::theme(legend.text = ggplot2::element_text(colour = general.color, size = legend.size)) +
#'             ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4)))
#'     }
#'     else
#'         p <- p + ggplot2::theme(legend.position = "none")
#'     p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(colour = general.color, size = axis.label.font.size))
#'     p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(colour = general.color, size = axis.label.font.size))
#'     p <- p + ggplot2::theme(axis.title.x = ggplot2::element_text(colour = general.color, size = axis.title.font.size))
#'     p <- p + ggplot2::theme(axis.title.y = ggplot2::element_text(colour = general.color, size = axis.title.font.size))
#'     p <- p + ggplot2::theme(axis.title = ggplot2::element_text(size = axis.title.font.size, face = "bold"))
#'     p <- p + ggplot2::theme(plot.title = ggplot2::element_text(size = main.size, face="bold", vjust = 1.2))
#'     p <- p + ggplot2::geom_hline(yintercept = 0, colour = general.color, linetype="dashed")
#'     p <- p + ggplot2::geom_vline(xintercept = 0, colour = general.color, linetype="dashed")
#'     p <- p + ggplot2::theme(legend.key = ggplot2::element_rect(colour = "NA"))
#'     #
#'     # connecting points to text using lines
#'     #
#'     for (i in 1:n) {
#'         x <- coords[i, 1]
#'         y <- coords[i, 2]
#'         x1 <- label.coords[i, 1]
#'         y1 <- label.coords[i, 2]
#'         w <- label.dimensions[i, 1]
#'         h <- label.dimensions[i, 2]
#'         cs <- BoxCoordinates(x1, y1, w, h)
#'         .absSlope <- function(x1, y1, x2, y2) abs((y1 - y2) / (x1 - x2))
#'         if (DistanceToBox(x, y, cs[1], cs[2], cs[3], cs[4], cs[5], cs[6], cs[7], cs[8]) > h) {
#'             line.color = colors[group[i]]
#'             if (.absSlope(x, y, x1, y1) < 0.5) {
#'                 if (x > x1) { # point to the righy of label
#'                     p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[3], y = y, yend = cs[2] + h / 2), colour = line.color, size = .5)#, size = 3)
#'             } else  { # point to the left of label
#'               p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1], y = y, yend = cs[2] + h / 2), colour = line.color, size = .5)#, colour = transparent.colors[i], size = 3)
#'             }
#'           } else {
#'             if (y > y1) { # point above the label
#'               p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1] + w / 2, y = y, yend=cs[8]), colour = line.color, size = .5)##, colour = transparent.colors[i], size = 3)
#'             } else  { # point below the label
#'               p <- p + ggplot2::geom_segment(ggplot2::aes_string(x = x, xend = cs[1] + w / 2, y = y, yend=cs[2]), colour = line.color, size = .5)##, colour = transparent.colors[i], size = 3)
#'             }
#'           }
#'         }
#'       }
#'       p
#' }
