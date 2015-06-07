library("iterators")
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
#lambda.default = c(40, 40, 40, 40, 40, 40)
lambda.default = rep(40, 6)

groups.number <- 4
needed.percentage = c(.03, .25, .35, .25, .12)

dist.min <- 20

calculate.lambda <- function(lambda.current) {
    result = rep(-1, n)
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
}

iterate.lambdas.seq <- function(lambda.current, lambda.pos) {
    #lambda.min <- -5
    #lambda.max <- 5
    # LAMBDA 40
    #if (lambda.pos == 2) {
    #    lambda.min <- -10
    #    lambda.max <- -5
    #} else if (lambda.pos == 3) {
    #    lambda.min <- 5
    #    lambda.max <- 10
    #} else if (lambda.pos == 4) {
    #    lambda.min <- -5
    #    lambda.max <- 0
    #} else if (lambda.pos == 5) {
    #    lambda.min <- -4
    #    lambda.max <- -1
    #} else if (lambda.pos == 6) {
    #    lambda.min <- -10
    #    lambda.max <- -5
    #}
    # LAMBDA 35
    if (lambda.pos == 2) {
        lambda.min <- 0
        lambda.max <- 5
    } else if (lambda.pos == 3) {
        lambda.min <- -5
        lambda.max <- 10
    } else if (lambda.pos == 4) {
        lambda.min <- -15
        lambda.max <- -5
    } else if (lambda.pos == 5) {
        lambda.min <- -5
        lambda.max <- 0
    } else if (lambda.pos == 6) {
        lambda.min <- -5
        lambda.max <- 0
    }

    get_lambda <- function (i) {
        lambda.current[lambda.pos] <- lambda.current[lambda.pos-1] + i
        return(lambda.current)
    }
    lambdas <- Map(get_lambda, lambda.min:lambda.max)
    #result <- foreach(i=lambdas) %do% {
    #    iterate.lambdas(i, lambda.pos+1)
    #}
    for (l in lambdas) {
        iterate.lambdas(l, lambda.pos+1)
    }
    return()
}

iterate.lambdas <- function(lambda.current, lambda.pos) {
    if (lambda.pos == 7) {
        calculate.lambda(lambda.current)
    } else {
        iterate.lambdas.seq(lambda.current, lambda.pos)
    }
    return()
}

lambda.current <- lambda.default
lambda.min <- 0
lambda.max <- 5
get_lambda <- function (i) {
    lambda.current[2] <- lambda.current[1] + i
    return(lambda.current)
}
lambdas <- Map(get_lambda, lambda.min:lambda.max)
cl <- makeCluster(detectCores(), outfile="")
registerDoParallel(cl, cores = detectCores() - 1)
foreach(i=lambdas, .options.multicore=c(silent=FALSE)) %dopar% {
    source("generate_sequence.r")
    source("analyze_approximation.r")
    source("chi_squared.r")
    options(warn=1)
    iterate.lambdas(i, 3)
}
stopCluster(cl)
