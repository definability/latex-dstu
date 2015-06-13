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

get.advice <- function(x) {
    if (is.element(x, c(groups.unclassified, groups.movable))) {
        'Більше думати'
    } else if (is.element(x, c(groups.weak, groups.inert))) {
        'Більше тренуватися'
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
