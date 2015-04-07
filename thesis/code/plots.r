library(NHPoisson)
source("analyze_approximation.r")
wideScreen()
# Read command line arguments
args <- commandArgs(TRUE)
display_approximation <- FALSE
if (length(args) > 0 && args[1] == "approximation") {
    display_approximation <- TRUE
}
# Init cells for charts
n <- 3
m <- 3
png('output.png', width=18, height=22, units="cm", res=300)
par(mfrow=c(n,m))
chart.xlab <- "Час"
chart.ylab <- "Кількість постукувань"
chart.ylim <- c(20,50)
# Parameters
sample.time <- seq(5, 30, 5)
sample.size = length(sample.time)
sample.prediction_degree = 2
sample.poly = poly(sample.time, sample.prediction_degree, raw=TRUE)
lambda.default = c(40, 38, 36, 34, 32, 30, 0)
# Fill every cell
for (i in 1:(n*m)) {
    # Generate non-homogeneous Poisson process
    sample.trajectory <- simNHP.fun(lambda=lambda.default)
    # Calculate number of events in every sample.time interval
    # (we have sample.size of them)
    events_on_step <- function(x) length(sample.trajectory[
                                         sample.trajectory$posNH==x])
    sample.current <- unlist((Map(events_on_step, 1:(sample.size))))
    # Plot
    plot(sample.time, sample.current, xlab=chart.xlab, ylab=chart.ylab,
         ylim=chart.ylim)
    lines(sample.time, sample.current)
    if (display_approximation) {
        # Calculate approximation
        sample.model <- lm(sample.current ~ sample.poly)
        sample.prediction <- predict(sample.model)
        lines(sample.time, sample.prediction, col='red')
        extrema <- find_extrema(sample.model, sample.time)
        abline(h=sample.model$coefficients[3]*sample.time[1]^2 +
                 sample.model$coefficients[2]*sample.time[1] +
                 sample.model$coefficients[1], col='green', lty=2, lwd=1)

        abline(v=extrema[1], col='gray', lty=3, lwd=1)
        abline(v=extrema[2], col='blue', lty=3, lwd=1)
    }
}
dev.off()
