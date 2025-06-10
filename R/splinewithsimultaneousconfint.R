#' Plots spline with confidence intervals
#'
#' @inheritParams flipStandardCharts::Line
#' @param outcome Outcome variable shown in the y-axis. This should be convertible using \code{AsNumeric}.
#' @param predictor Predictor variable shown in the x-axis. This should be a numeric or date variable.
#' @param type Regression link function used. Currently can be "Binary Logit" or "Linear".
#' @param subset Optional logical vector of the same length as \code{outcome}
#'  and \code{predictor} specifying whether the observation should be included
#'  in the analysis.
#' @param weights Optional numeric vector used as sampling weights.
#' @param seed Random seed for reproducibility
#' @param confidence Width of confidence interval shown around the spline.
#' @param ci.color Color of the ribbon showing the confidence interval.
#' @param number.draws Number of possible trend lines to super-impose.
#' @param draw.color Color of the possible trend lines.
#' @param draw.weight Line weight of the possible trend line.
#' @param mean.color Line color of the mean (or predicted) line.
#' @param mean.weight Line weight of the mean line.
#' @param font.units One of "px" of "pt". By default all font sizes are specified in terms of
#' pixels ("px"). But changing this to "pt" will mean that the font sizes will be in terms
#' points ("pt"), which will be consistent with font sizes in text boxes.
#' @param trim.padding (Deprecated)Logical; whether to remove padding around plotly chart.
#'   Default is set to false so that output is the same as old charts.
#' @param ... Other parameters to pass to flipStandardCharts::Line.
#' @importFrom flipStandardCharts Line
#' @importFrom flipTransformations AsNumeric AdjustDataToReflectWeights DichotomizeFactor
#' @importFrom flipU StopForUserError
#' @importFrom mgcv gam mroot
#' @importFrom plotly plot_ly add_ribbons add_lines layout config
#' @importFrom utils stack
#' @importFrom scales percent
#' @importFrom stats binomial coef complete.cases family predict quantile rnorm vcov gaussian
#' @importFrom verbs Sum
#' @export
SplineWithSimultaneousConfIntervals <- function(outcome,
                                                predictor,
                                                type = "Binary Logit",
                                                subset = NULL,
                                                weights = NULL,
                                                seed = 42,
                                                number.draws = 30,
                                                confidence = 0.95,
                                                mean.color = "#000000",
                                                mean.weight = 2,
                                                draw.color = "#33333366",
                                                draw.weight = 1,
                                                ci.color = "#FF000033",
                                                y.tick.format = NULL,
                                                x.grid.width = 1,
                                                y.hovertext.format = ".1f",
                                                title = NULL,
                                                x.title = NULL,
                                                y.title = NULL,
                                                global.font.family = "Open Sans",
                                                global.font.color = rgb(44, 44, 44, maxColorValue = 255),
                                                title.font.size = 12,
                                                hovertext.font.size = 9,
                                                y.title.font.size = 10,
                                                x.title.font.size = 10,
                                                y.tick.font.size = 9,
                                                x.tick.font.size = 9,
                                                font.units = "pt",
                                                trim.padding = FALSE,
                                                ...)
{
    if (length(unique(outcome)) == 1)
        StopForUserError("Could not construct model as outcome variable contains only one value.")
    if (type == "Binary Logit")
    {
        # If outcome variable is too unbalanced then binary logit model
        # cannot be fitted anyway so no need to continue
        twolevel <- try(DichotomizeFactor(as.factor(outcome)), silent = TRUE)
        if (inherits(twolevel, "try-error"))
            StopForUserError("Outcome variable cannot be dichotimized (e.g. perhaps only has 1 value).")
        tidy.outcome <- twolevel == levels(twolevel)[2]
        attr(outcome, "label") <- paste(attr(outcome, "label"), levels(twolevel)[2])
    } else
        tidy.outcome <- AsNumeric(outcome, binary = FALSE)

    data <- data.frame(outcome = tidy.outcome,
                      predictor = predictor,
                      predictor.numeric = scale(as.numeric(predictor)))
    logit <- type == "Binary Logit"

    # Filters and weights
    if (length(subset) > 1)
    {
        if (length(subset) != nrow(data))
            StopForUserError("Filter does not have the same length as the input data. Select a variable from the same data set.")
        data <- subset(data, subset)
        weights <- subset(weights, subset)
    }
    if (!is.null(weights))
    {
        data <- try(AdjustDataToReflectWeights(data, weights))
        if (inherits(data, "try-error"))
            StopForUserError("Could not create dataset of ", ceiling(Sum(weights, remove.missing = FALSE)),
                             " observations as specified by the weights")
    }

    data <- data[complete.cases(data), ]
    ylab <- attr(outcome, "label")
    xlab <- attr(predictor, "label")

    # Fitting the model
    m <- gam(outcome ~ s(predictor.numeric), data = data,
             family = if (logit) binomial(link="logit") else gaussian(link="identity"),
             method = "REML")
    newd <- with(data,
                 data.frame(predictor.numeric = seq(min(predictor.numeric, na.rm = TRUE),
                                max(predictor.numeric, na.rm = TRUE), length = 200),
                            predictor = seq(min(predictor, na.rm = TRUE),
                                max(predictor, na.rm = TRUE), length = 200)))
    if (inherits(newd$predictor, "POSIXct"))
        newd$predictor <- as.POSIXct(newd$predictor, origin = "1970-01-01")
    pred <- predict(m, newd, type = "link", se.fit = TRUE)
    pred$predictor <- newd$predictor

    # Sample from MVN random deviates
    rmvn <- function(n, mu, sig)
    {
        L <- mroot(sig)
        m <- ncol(L)
        t(mu + L %*% matrix(rnorm(m*n), m, n))
    }
    set.seed(seed)
    N <- 10000
    Vb <- vcov(m)
    BUdiff <- rmvn(N, mu = rep(0, nrow(Vb)), sig = Vb)
    Cg <- predict(m, newd, type = "lpmatrix")
    simDev <- Cg %*% t(BUdiff)
    absDev <- abs(sweep(simDev, 1, pred$se.fit, FUN = "/"))
    masd <- apply(absDev, 2L, max)
    crit <- quantile(masd, prob = confidence, type = 8)
    pred <- data.frame(data.frame(pred), newd,
                       uprS = pred$fit + (crit * pred$se.fit),
                       lwrS = pred$fit - (crit * pred$se.fit))
    if (logit)
    {
        invlink <- family(m)$linkinv
        pred$fit <- invlink(pred$fit)
        pred$uprS <- invlink(pred$uprS)
        pred$lwrS <- invlink(pred$lwrS)
    }
    sims <- rmvn(N, mu = coef(m), sig = Vb)
    fits <- Cg %*% t(sims)

    if (is.null(title))
        title <- paste0("Simultaneous ", confidence * 100, "% confidence intervals for fitted GAM")
    if (is.null(x.title))
        x.title <- xlab
    if (is.null(y.title))
        y.title <- ylab
    if (is.null(y.tick.format) && logit)
        y.tick.format <- "%"

    # For the standard charts, the font size conversion happens inside flipChart::CChart
    if (tolower(font.units) %in% c("pt", "point", "points"))
    {
        fsc <- 1.3333
        title.font.size = round(fsc * title.font.size, 0)
        hovertext.font.size = round(fsc * hovertext.font.size, 0)
        y.title.font.size = round(fsc * y.title.font.size, 0)
        x.title.font.size = round(fsc * x.title.font.size, 0)
        y.tick.font.size = round(fsc * y.tick.font.size, 0)
        x.tick.font.size = round(fsc * x.tick.font.size, 0)
    }

    # Create plot
    plot.data <- pred$fit
    names(plot.data) <- pred$predictor
    cat("plot.data:\n")
    print(plot.data)
    pp <- Line(plot.data, colors = mean.color, line.thickness = mean.weight,
             title = title, x.title = x.title, y.title = y.title,
             y.tick.format = y.tick.format, y.hovertext.format = y.hovertext.format,
             global.font.family = global.font.family, global.font.color = global.font.color,
             title.font.size = title.font.size, x.grid.width = x.grid.width,
             y.title.font.size = y.title.font.size, x.title.font.size = x.title.font.size,
             y.tick.font.size = y.tick.font.size, x.tick.font.size = x.tick.font.size,
             hovertext.font.size = hovertext.font.size, legend.show = FALSE, ...)

    cat("ribbon:\n")
    print(pred$predictor)
    pp$htmlwidget <- add_ribbons(pp$htmlwidget, x = names(plot.data),
                     ymin = pred$lwrS, ymax = pred$uprS,
                     fillcolor = ci.color, line = list(color = "transparent"),
                     text = sprintf(paste0("[%", y.hovertext.format, ", %", y.hovertext.format, "]"), pred$lwrS, pred$uprS),
                     hovertemplate = "%{x}: %{text}",
                     hoverlabel = list(font = list(color = flipStandardCharts:::autoFontColor(ci.color))),
                     name = "Confidence Interval")

    if (number.draws > 0)
    {
        rnd <- sample(N, number.draws)
        stackFits <- if (logit) stack(as.data.frame(invlink(fits[, rnd])))
                     else       stack(as.data.frame(fits[, rnd]))
        stackFits <- transform(stackFits, predictor.numeric = rep(newd$predictor.numeric, length(rnd)))
        stackFits$predictor = newd$predictor
        for (i in unique(stackFits$ind))
        {
            ind <- which(stackFits$ind == i)
            print(stackFits$predictor[ind][1:10])
            pp$htmlwidget <- add_lines(pp$htmlwidget, x = names(plot.data), y = stackFits$values[ind],
                      line = list(color = draw.color, width = draw.weight),
                      hoverlabel = list(font = list(color = flipStandardCharts:::autoFontColor(draw.color))),
                      name = paste("Draw", i))
        }
    }
    pp
}
