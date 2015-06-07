source("generate_sequence.r")
source("analyze_approximation.r")
source("modify_parabola.r")

task.H <- 500
task.N <- 10
tasks  <- rep(task.H, task.N)

exam.cpm     <- 1
exam.minutes <- 90
exam.length  <- exam.cpm * exam.minutes

exam.pass <- function (intensity) {
    get.tau <- function (t) {
        if (intensity(t)<=0) {
            return(0)
        }
        return(rexp(1, intensity(t)))
    }
    exam.pass.task <- function (time.passed, task) {
        tau <- get.tau(sum(time.passed))
        return(c(time.passed, tau*task))
    }
    Reduce(exam.pass.task, tasks, 0)
}

exam.get.row <- function (exam.result) {
    task.to.row <- function (task.time, task.number) {
        rep(task.number, ceiling(task.time/exam.cpm))
    }
    result <- head(unlist(Map(task.to.row, exam.result, 0:task.N)), exam.length)
    if (length(result) < exam.length) {
        return(c(result, rep(0, exam.length-length(result))))
    }
    return(result)
}

options(warn=1)
lambda.default = c(40, 41, 46, 38, 35, 34)

get.some <- function (n) {
    result <- c()
    for (i in 1:n) {
        sample.current <- generate_trajectory(lambda.default)
        abc <- find_abc(sample.current)
        #abc <- c(0.02214286, -1.04357143, 53.70000000)
        #get_group(abc[1], abc[2], abc[3])

        result <- rbind(result, exam.get.row(exam.pass(parabola.stretch(abc[1], abc[2], abc[3], exam.length))))
    }
    return(result)
}

result <- c()
for (i in 1:1000) {
    sample.current <- generate_trajectory(lambda.default)
    abc <- find_abc(sample.current)
    #abc <- c(0.02214286, -1.04357143, 53.70000000)
    #get_group(abc[1], abc[2], abc[3])

    result <- rbind(result, exam.get.row(exam.pass(parabola.stretch(abc[1], abc[2], abc[3], exam.length))))
}

draw.some <- function(i) {
    plot(density(result %*% ir.pca$rotation[,i]), col='red')
    polygon(density(result %*% ir.pca$rotation[,i]), col='red', border='blue')
}

#print(cov(result))
fit <- princomp(result)
print(summary(fit)) # print variance accounted for 
loadings(fit) # pc loadings 
ir.pca <- prcomp(result, center = TRUE)
#print(ir.pca)
#png('output.png', width=22, height=22, units="cm", res=100)
plot(ir.pca, type = "l")
#plot(fit,type="lines") # scree plot 
#fit$scores # the principal components
#biplot(fit)
