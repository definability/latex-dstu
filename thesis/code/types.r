library(NHPoisson)
source("generate_sequence.r")
source("analyze_approximation.r")

n       <- 100
rows    <- 3
columns <- 2
sample.time <- seq(5, 30, 5)
sample.size = length(sample.time)
sample.prediction_degree = 2
sample.poly = poly(sample.time, sample.prediction_degree, raw=TRUE)
lambda.default = c(40, 38, 36, 34, 32, 30, 0)
#lambda.default = rep(40, sample.size+1)

png('output.png', width=18, height=22, units="cm", res=300)
par(mfrow=c(rows,columns))

result = rep(-1, n)
for (j in 2:(rows*columns+1)) {
    set.seed(j)
    for (i in 1:n) {
        ## Generate non-homogeneous Poisson process trajectory
        sample.current <- generate_trajectory(lambda.default)
        # Calculate approximation
        sample.model <- lm(sample.current ~ sample.poly)
        sample.prediction <- predict(sample.model)
        extrema <- find_extrema(sample.model, sample.time)
        # Display group
        result[i] = get_group(sample.model, sample.time,
                              extrema[['min']], extrema[['max']])
    }
    elements_in_group <- function(x) length(result[result==x])+1
    groups <- unlist(Map(elements_in_group, -1:3))

    #print(paste("Undefined", round(groups[1]/n, 2)))
    #print(paste("Flat",      round(groups[2]/(n - groups[1]), 2)))
    #print(paste("Weak",      round(groups[3]/(n - groups[1]), 2)))
    #print(paste("Middle",    round(groups[4]/(n - groups[1]), 2)))
    #print(paste("Movable",   round(groups[5]/(n - groups[1]), 2)))
    print(groups)
    barplot(groups, ylim=c(1, 70), log='y',
            names.arg=c("Помилки", "Прямі", "Спадні", "Проміжні", "Опуклі"))
}
