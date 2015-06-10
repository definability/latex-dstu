library('ggplot2')

source("generate_sequence.r")
source("analyze_approximation.r")
source("modify_parabola.r")

exam.cpm     <- 6
exam.minutes <- 90
exam.length  <- exam.cpm * exam.minutes

task.N <- 3
task.H <- 10 * exam.length / task.N
tasks  <- rep(task.H, task.N)

exam.pass <- function (intensity) {

    get.tau <- function (t) {
        return(rexp(1, intensity(t)))
    }

    exam.H.passed <- Reduce(function (intensity.accumulated, intensity.current) {
        c(intensity.accumulated, intensity.accumulated[length(intensity.accumulated)] + intensity.current)
    }, Map(intensity, 1:exam.length))

    exam.row <- function () {
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
    }

    exam.row()
}

options(warn=1)
lambda.default = c(40, 41, 46, 38, 35, 34)

get.some <- function (n) {
    result <- c()
    groups <- rep(-1, n)
    for (i in 1:n) {
        sample.current <- generate_trajectory(lambda.default)
        abc <- find_abc(sample.current)
        groups[i] <- get_group(abc[1], abc[2], abc[3])
        intensity <- parabola.stretch(abc[1], abc[2], abc[3], exam.length)
        result <- c(result, exam.pass(intensity))
    }
    list(values=matrix(result, ncol=exam.length, byrow = T), groups=groups)
}

draw.some <- function(i) {
    plot(density(result$values %*% ir.pca$rotation[,i]), col='red')
    polygon(density(result$values %*% ir.pca$rotation[,i]), col='red', border='blue')
}

get.group <- function (sample, group.number) {
    sample$values[sample$groups == group.number,]
}

groups.names <- c("Не класиф.", "Зміш.", "Слабкий",
                  "Неврів.", "Рухливий", "Інерт.")
get.chart.g <- function(sample, i, groups) {
    vegLengths <- Reduce(function (result, g) {
        rbind(result, data.frame(
              length=c(get.group(sample, g) %*% ir.pca$rotation[,i]),
              veg=groups.names[g+2], stringsAsFactors=FALSE))
    }, groups, c())
    ggplot(vegLengths, aes(length, fill=veg)) + geom_density(alpha = 0.2)
}

get.chart <- function(sample, i) {
    get.chart.g(sample, i, (seq(length(groups.names))-2))
}

get.amount <- function(sample, pc, group.number, f) {
    fpc <- get.group(sample, group.number) %*% ir.pca$rotation[,pc]
    fpc[f(fpc)]
}

result <- get.some(1000)

ir.pca <- prcomp(result$values, center = TRUE)
plot(ir.pca, type = "l")
