library('ggplot2')

source("generate_sequence.r")
source("analyze_approximation.r")
source("modify_parabola.r")

#exam.cpm     <- 1
exam.cpm     <- 6
exam.minutes <- 90
exam.length  <- exam.cpm * exam.minutes

task.N <- 10
task.H <- 15 * exam.length / task.N
tasks  <- rep(task.H, task.N)

exam.pass <- function (intensity) {

    get.tau <- function (t) {
        #return(rexp(1, intensity(t)))
        intensity(t)
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
        result <- rbind(result, exam.pass(intensity))
    }
    list(values=result, groups=groups)
}

draw.some <- function(i) {
    plot(density(result$values %*% ir.pca$rotation[,i]), col='red')
    polygon(density(result$values %*% ir.pca$rotation[,i]), col='red', border='blue')
}

get.group <- function (sample, group.number) {
    sample$values[sample$groups == group.number,]
}

get.chart <- function(sample, i) {
    groups.names <- c("Не класиф.", "Зміш.", "Слабкий",
                      "Неврів.", "Рухливий", "Інерт.")
    vegLengths <- Reduce(function (result, g) {
        rbind(result, data.frame(
              length=c(get.group(sample, g) %*% ir.pca$rotation[,i]),
              veg=groups.names[g+2], stringsAsFactors=FALSE))
    }, (seq(length(groups.names))-2))
    ggplot(vegLengths, aes(length, fill=veg)) + geom_density(alpha = 0.2)
}


#PC1 <- data.frame(length=c(result %*% ir.pca$rotation[,1]))
#PC1$veg <- 'PC1'
#PC2 <- data.frame(length=c(result %*% ir.pca$rotation[,2]))
#PC2$veg <- 'PC2'
#PC3 <- data.frame(length=c(result %*% ir.pca$rotation[,3]))
#PC3$veg <- 'PC3'
#PC4 <- data.frame(length=c(result %*% ir.pca$rotation[,4]))
#PC4$veg <- 'PC4'
#PC5 <- data.frame(length=c(result %*% ir.pca$rotation[,5]))
#PC5$veg <- 'PC5'
#PC6 <- data.frame(length=c(result %*% ir.pca$rotation[,6]))
#PC6$veg <- 'PC6'
#vegLengths <- rbind(PC1, PC2, PC3, PC4, PC5, PC6)
#ggplot(vegLengths, aes(length, fill=veg)) + geom_density(alpha = 0.2)

Rprof('profile.out')
result <- get.some(1000)
Rprof(NULL)
summaryRprof("profile.out")

#print(cov(result))
#fit <- princomp(result)
#print(summary(fit)) # print variance accounted for 
#loadings(fit) # pc loadings 
ir.pca <- prcomp(result$values, center = TRUE)
#print(ir.pca)
#png('output.png', width=22, height=22, units="cm", res=100)
plot(ir.pca, type = "l")
#plot(fit,type="lines") # scree plot 
#fit$scores # the principal components
#biplot(fit)
