library(NHPoisson)
source("generate_sequence.r")
source("analyze_approximation.r")
wideScreen()
# Read command line arguments
args <- commandArgs(TRUE)
display_approximation <- TRUE
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
lambda.default = c(40, 44, 48, 40, 36, 34, 0)
# Fill every cell
for (i in 1:(n*m)) {
    # Generate non-homogeneous Poisson process trajectory
    sample.current <- generate_trajectory(lambda.default)
    # Plot
    plot(sample.time, sample.current, xlab=chart.xlab, ylab=chart.ylab,
         ylim=chart.ylim)
    lines(sample.time, sample.current)
    if (display_approximation) {
        # Calculate approximation
        abc <- find_abc(sample.current)
        f <- function(t) {
            return(abc[1]*t^2+abc[2]*t+abc[3])
        }
        # Draw approximation
        lines(sample.time, Map(f, sample.time), xlab=chart.xlab, ylab=chart.ylab,
         ylim=chart.ylim, col='red')
        # Draw start rhythm
        #abline(h=sample.model$coefficients[3]*sample.time[1]^2 +
        #         sample.model$coefficients[2]*sample.time[1] +
        #         sample.model$coefficients[1], col='green', lty=2, lwd=1)
        # Draw minumum and maximum
        #abline(v=extrema[['min']], col='gray', lty=3, lwd=1)
        #abline(v=extrema[['max']], col='blue', lty=3, lwd=1)
        # Display group
        #print(get_group(sample.model, sample.time,
        #                extrema[['min']], extrema[['max']]))
    }
}
dev.off()
