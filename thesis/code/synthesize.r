library("parallel")
library("foreach")
library("doParallel")

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
lambda.default = c(40, 40, 40, 40, 40, 40)

needed.percentage = c(.05, .25, .35, .25, .10)

groups.number = 4
groups.names = c("Не класиф.", "Зміш.", "Слабкий", "Неврів.", "Рухливий", "Інерт.")

global.groups = rep(0, length(groups.number))
global.percentage = rep(0, length(groups.number))
global.distances = rep(0, length(groups.number))

png('output.png', width=22, height=22, units="cm", res=300)
par(mfrow=c(rows,columns))

result = rep(-1, n)
res <- rep(0, groups.number+2)
dist.min <- n*.1
iterate.lambdas <- function(lambda.current, lambda.pos=2) {
    result <- c()
    if (lambda.pos == 7) {
        for (i in 1:n) {
            ## Generate non-homogeneous Poisson process trajectory
            sample.current <- generate_trajectory(lambda.current)
            # Calculate approximation
            abc <- find_abc(sample.current)
            # Get group
            result[i] = get_group(abc[1], abc[2], abc[3])
        }
        elements_in_group <- function(x) length(result[result==x])
        groups <- unlist(Map(elements_in_group, 0:groups.number))
        distance <- chi_squared.distance(n, groups/n, needed.percentage)
        if (distance < dist.min) {
            cat("Lambda:", sprintf("%d,", lambda.current),
                sprintf("Distance: %.2f,", distance),
                "Groups:", sprintf("%.2f,", groups/n),
                "\n")

            dist.min <<- distance
        }
        return()
    } else {
        lambda.min <- -5
        lambda.max <- 5
        if (lambda.pos == 2) {
            lambda.min <- -9
            lambda.max <- 0
        } else if (lambda.pos == 3) {
            lambda.min <- 0
            lambda.max <- 9
        } else if (lambda.pos > 3) {
            lambda.min <- -9
            lambda.max <- 0
        }
        get_lambda <- function (i) {
            lambda.current[lambda.pos] <- lambda.current[lambda.pos-1] + i
            return(lambda.current)
        }
        lambdas <- Map(get_lambda, lambda.min:lambda.max)
        result <- foreach(i=lambdas) %do% {
            iterate.lambdas(i, lambda.pos+1)
        }
        return()
    }
}

lambda.min <- -9
lambda.max <- 0
get_lambda <- function (i) {
    lambda.default[2] <- lambda.default[1] + i
    return(lambda.default)
}
lambdas <- Map(get_lambda, lambda.min:lambda.max)
cl <- makeCluster(detectCores(), outfile="")
registerDoParallel(cl, cores = detectCores() - 1)
foreach(i=lambdas, .combine=rbind,
                  .options.multicore=c(silent=FALSE),
                  .packages=c('foreach')) %dopar% {
    source("generate_sequence.r")
    source("analyze_approximation.r")
    source("chi_squared.r")
    iterate.lambdas(i, 3)
}
stopCluster(cl)
