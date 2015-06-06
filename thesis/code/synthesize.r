library(NHPoisson)
source("generate_sequence.r")
source("analyze_approximation.r")
source("chi_squared.r")

n       <- 1000
rows    <- 3
columns <- 2
sample.time <- seq(5, 30, 5)
sample.size = length(sample.time)
sample.prediction_degree = 2
sample.poly = poly(sample.time, sample.prediction_degree, raw=TRUE)
lambda.default = c(40, 40, 40, 40, 40, 40, 0)

needed.percentage = c(.01, .04, .25, .35, .25, .10)

groups.number = 4
groups.names = c("Не класиф.", "Зміш.", "Слабкий", "Неврів.", "Рухливий", "Інерт.")

global.groups = rep(0, length(groups.number))
global.percentage = rep(0, length(groups.number))
global.distances = rep(0, length(groups.number))

png('output.png', width=22, height=22, units="cm", res=300)
par(mfrow=c(rows,columns))

result = rep(-1, n)
res <- rep(0, groups.number+2)
dist.min <- 1000000
iterate.lambdas <- function(lambda.current, lambda.pos=2) {
    result <- c()
    if (lambda.pos == 7) {
        for (i in 1:n) {
            ## Generate non-homogeneous Poisson process trajectory
            sample.current <- generate_trajectory(lambda.current)
            # Calculate approximation
            abc <- find_abc(sample.current)
            # Get group
            result[i] = get_group(abc[1], abc[2], abc[3], sample.time)
        }
        elements_in_group <- function(x) length(result[result==x])
        groups <- unlist(Map(elements_in_group, -1:groups.number))
        distance <- chi_squared.distance(n, groups/n, needed.percentage)
        if (distance < dist.min) {
            print(distance)
            print(lambda.current)
            dist.min <<- distance
        }
        return(distance)
    }
    min <- -5
    max <- 5
    for (i in min:max) {
        lambda.i <- lambda.current
        lambda.i[lambda.pos] <- lambda.current[lambda.pos] + i
        iterate.lambdas(lambda.i, lambda.pos+1)
    }
}
iterate.lambdas(lambda.default, 3)
