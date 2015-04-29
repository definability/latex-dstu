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
groups.number = 4
groups.names = c("Помилки", "Прямі", "Спадні", "Проміжні", "Опуклі", "Ввігнуті")
#lambda.default = rep(40, sample.size+1)

png('output.png', width=22, height=22, units="cm", res=300)
par(mfrow=c(rows,columns))

result = rep(-1, n)
res <- rep(0, groups.number+2)
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
    elements_in_group <- function(x) length(result[result==x])
    groups <- unlist(Map(elements_in_group, -1:groups.number))

    print(groups)
    res[[1]] = res[[1]] + groups[[1]]
    res[[2]] = res[[2]] + groups[[2]]
    res[[3]] = res[[3]] + groups[[3]]
    res[[4]] = res[[4]] + groups[[4]]
    res[[5]] = res[[5]] + groups[[5]]
    res[[6]] = res[[6]] + groups[[6]]
    barplot(groups, ylim=c(0, 70), names.arg=groups.names)
}
print(res/600)
