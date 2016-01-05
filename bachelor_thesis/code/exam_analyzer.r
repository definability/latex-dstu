library('party')

groups <- -1:4

groups.unclassified <- -1
groups.neutral <- 0
groups.weak <- 1
groups.unstable <- 2
groups.movable <- 3
groups.inert <- 4

groups.investigated <- c(groups.unclassified, groups.weak, groups.movable,
                     groups.inert)

advice.think.more <- 'Більше думати'
advice.learn.more  <- 'Більше тренуватися'

get.advice <- function(x) {
    if (is.element(x, c(groups.unclassified, groups.movable))) {
        advice.think.more
    } else if (is.element(x, c(groups.weak, groups.inert))) {
        advice.learn.more
    } else {
        NA
    }
}

get.students <- function (sample, rotation) {
    investigated.indices <- which(is.element(sample$groups,
                                             groups.investigated))
    advices <- unlist(Map(get.advice, sample$groups[investigated.indices]))
    data.frame(group=advices,
               ГК1=sample$values[investigated.indices,]%*%rotation)
}

get.fit <- function (sample) {
    ctree(group ~ ГК1, data=sample, controls=ctree_control(maxdepth=1))
}

get.pc.proportion <- function (standard.deviations, number) {
    standard.deviations[number]^2/sum(standard.deviations^2)
}

get.prediction.quality <- function (cart, exam) {
    advices <- unlist(Map(get.advice, exam$groups))
    indices <- which(Negate(is.na)(advices))
    prediction <- predict(students.cart, data.frame(ГК1=exam$values[indices,] %*% students.pca$rotation[,1]))
    think.more <- prediction[advices[indices] == advice.think.more] == advice.think.more
    learn.more <- prediction[advices[indices] == advice.learn.more] == advice.learn.more
    list(think=sum(think.more)/length(think.more),
         learn=sum(learn.more)/length(learn.more))
}
