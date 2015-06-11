library('ggplot2')
library('party')

source("generate_sequence.r")
source("analyze_approximation.r")
source("modify_parabola.r")
options(warn=1)

lambda.default = c(40, 41, 46, 38, 35, 34)

exam.cpm     <- 6
exam.minutes <- 90
exam.length  <- exam.cpm * exam.minutes

task.N <- 10
#task.H <- exam.length / (task.N*mean(lambda.default))
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

get.some <- function (n) {
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

draw.some <- function(i) {
    plot(density(result$values %*% students.pca$rotation[,i]), col='red', main=paste(i, "головна компонента"))
    polygon(density(result$values %*% students.pca$rotation[,i]), col='red', border='blue')
}

get.group <- function (sample, group.number) {
    sample$values[sample$groups == group.number,]
}

groups.names <- c("Не класиф.", "Зміш.", "Слабкий",
                  "Неврів.", "Рухливий", "Інерт.")

get.chart.g <- function(sample, i, groups) {
     pca.distribution <- Reduce(function (result, g) {
        rbind(result, data.frame(
              Значення=c(get.group(sample, g) %*% students.pca$rotation[,i]),
              Тип=groups.names[g+2], stringsAsFactors=FALSE))
    }, groups, c())
    ggplot(pca.distribution, aes(Значення, fill=Тип)) +
           ylab("Щільність") +
           geom_density(alpha = 0.2)
}

get.chart <- function(sample, i) {
    get.chart.g(sample, i, (seq(length(groups.names))-2))
}

get.amount <- function(sample, pc, group.number, f) {
    fpc <- get.group(sample, group.number) %*% students.pca$rotation[,pc]
    fpc[f(fpc)]
}

result <- get.some(600)

students.pca <- prcomp(result$values, center = TRUE)
plot(students.pca, type = "l", title="Головні компоненти")
print(summary(students.pca))

get.strict.pc <- function (sample, pc) {
    sample$values[is.element(sample$groups, c(-1, 1, 3, 4)),]%*%students.pca$rotation[,1]
}

get.students <- function (sample) {
    data.frame(group=unlist(Map(
    function(x) {
        if (x == -1 || x == 3) {
            'Більше думати'
        } else if (x == 1 || x == 4) {
            'Більше тренуватися'
        } else {
            NA
        }
    },
    sample$groups[is.element(sample$groups, c(-1, 1, 3, 4))])),
    ГК1=get.strict.pc(sample, 1))
}

students <- data.frame(
    group=unlist(Map(function(x) {
        if (x == -1 || x == 3) {
            'Більше думати'
        } else if (x == 1 || x == 4) {
            'Більше тренуватися'
        } else {
            NA
        }
    },
    result$groups[is.element(result$groups, c(-1, 1, 3, 4))])),
    ГК1=get.strict.pc(result, 1))


fit.visual <- ctree(group ~ ГК1, data=students, controls=ctree_control(maxdepth=1))
fit.good <- ctree(group ~ ГК1, data=students)

plot(fit.visual, main="Дерево класифікації студентів")

#treeresponse(fit.good, newdata=students[1:10,])
