source("generate_sequence.r")
source("modify_parabola.r")

options(warn=1)

lambda.default = c(40, 41, 46, 38, 35, 34)

exam.cpm     <- 6
exam.minutes <- 90
exam.length  <- exam.cpm * exam.minutes

task.N <- 10
task.H <- exam.length / (task.N*mean(lambda.default)*3)
tasks  <- rep(task.H, task.N)

exam.pass <- function (intensity) {
    get.tau <- function (t) {
        result <- rexp(1, intensity(t))
        if (is.nan(result)) print(t)
        result
    }
    exam.H.passed <- rep(NA, exam.length)
    exam.H.passed[1] <- get.tau(1)
    for (i in 2:exam.length) {
        exam.H.passed[i] <- get.tau(i) + exam.H.passed[i-1]
    }
    (function () {
        is.positive <- function (x) {
            x > 0
        }
        .exam.row <- function (accumulator, task.n.current, exam.H.left) {
            tmp <- exam.H.left - task.H
            positive <- tmp[is.positive(tmp)]
            negative <- tmp[Negate(is.positive)(tmp)]
            if (length(positive) == 0 || task.n.current == task.N) {
                c(accumulator, rep(task.n.current, length(negative)), rep(0, length(positive)))
            } else {
                .exam.row(c(accumulator, rep(task.n.current, length(negative))), task.n.current+1, positive)
            }
        }
        .exam.row(c(), 1, exam.H.passed)
    })()
}

exam.generate <- function (n) {
    result <- c()
    groups <- rep(-1, n)
    values <- matrix(nrow=n, ncol=exam.length)
    abc <- NA
    for (i in 1:n) {
        repeat {
            abc <- find_abc(generate_trajectory(lambda.default))
            if (abc[3] > 0) break
        }
        new.parabola <- parabola.stretch(abc[1], abc[2], abc[3], exam.length)
        groups[i] <- get_group(abc[1], abc[2], abc[3])
        values[i,] <- exam.pass(new.parabola)
    }
    list(values=values, groups=groups)
}

