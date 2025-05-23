#' Plots spline with confidence intervals
#'
#' @param outcome Outcome variable shown in the y-axis. This should be convertible using \code{AsNumeric}.
#' @param predictor Predictor variable shown in the x-axis. This should be a numeric or date variable.
#' @param type Regression link function used. Currently can be "Binary Logit" or "Linear".
#' @param subset Optional logical vector of the same length as \code{outcome}
#'  and \code{predictor} specifying whether the observation should be included
#'  in the analysis.
#' @param weights Optional numeric vector used as sampling weights.
#' @param seed Random seed for reproducibility
#' @param confidence Width of confidence interval shown around the spline.
#' @param number.draws Number of possible trend lines to super-impose.
#' @param trim.padding Logical; whether to remove padding around plotly chart.
#'   Default is set to false so that output is the same as old charts.
#' @importFrom flipTransformations AsNumeric AdjustDataToReflectWeights DichotomizeFactor
#' @importFrom flipU StopForUserError
#' @importFrom mgcv gam mroot
#' @importFrom ggplot2 ggplot geom_ribbon geom_path aes theme_set theme_bw labs scale_y_continuous
#' @importFrom plotly ggplotly config
#' @importFrom utils stack
#' @importFrom scales percent
#' @importFrom rlang .data
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
                                                trim.padding = FALSE)
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

    # Create plot
    theme_set(theme_bw())
    p <- ggplot(pred, aes(x = .data$predictor, y = .data$fit)) +
            geom_ribbon(aes(ymin = .data$lwrS, ymax = .data$uprS), alpha = 0.2, fill = "red") +
            geom_path(lwd = 2) +
            labs(y = ylab, x = xlab,
                 title = paste0("Simultaneous ", confidence * 100, "% confidence intervals for fitted GAM"),
                 subtitle = sprintf("Each line is one of %i draws from the Bayesian posterior distribution of the model", number.draws))

    if (number.draws > 0)
    {
        rnd <- sample(N, number.draws)
        stackFits <- if (logit) stack(as.data.frame(invlink(fits[, rnd])))
                     else       stack(as.data.frame(fits[, rnd]))
        stackFits <- transform(stackFits, predictor.numeric = rep(newd$predictor.numeric, length(rnd)))
        stackFits$predictor = newd$predictor
        p = p + geom_path(data = stackFits,
                mapping = aes(y = .data$values, x = .data$predictor, group = .data$ind),
                alpha = 0.4, colour = "grey20")
    }
    if (logit)
        p <- p + scale_y_continuous(labels = percent)
    p <- ggplotly(p)
    p <- config(p, displayModeBar = FALSE)
    p$sizingPolicy$browser$padding <- if (trim.padding) 0 else 40
    p
}
