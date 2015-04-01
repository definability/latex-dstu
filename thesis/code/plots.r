library(NHPoisson)
wideScreen();
# Read command line arguments
args <- commandArgs(TRUE);
displayApproximation <- FALSE;
if (length(args) > 0 && args[1] == "approximation") {
    displayApproximation <- TRUE;
}
# Init cells for charts
n <- 3;
m <- 3;
png('output.png', width=18, height=22, units="cm", res=300);
par(mfrow=c(n,m));
# Parameters
sample.size = 6;
time <- seq(5, 30, 5);
lambda.default = c(40, 38, 36, 34, 32, 30, 0);
# Fill every cell
for (i in 1:(n*m)) {
    # Generate non-homogeneous Poisson process
    aux <- simNHP.fun(lambda=lambda.default);
    # Calculate number of events in every time interval (we have sample.size of them)
    cur <- unlist((Map(function(x) length(aux$posNH[aux$posNH==x]), 1:(sample.size))));
    # Calculate approximation
    model <- lm(c(cur) ~ poly(time, 2, raw=TRUE));
    predictedcounts <- predict(model);
    # Plot
    plot(time, c(cur), xlab="Час", ylab="Кількість постукувань", ylim=c(20,50));
    lines(time, c(cur));
    if (displayApproximation) {
        lines(time, predictedcounts, col='red');
    }
}
dev.off();
