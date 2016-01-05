library('ggplot2')

source("generate_sequence.r")
source("analyze_approximation.r")
source("chi_squared.r")
source("exam_draw.r")

n       <- 100
rows    <- 3
columns <- 2
sample.time <- seq(5, 30, 5)
sample.size = length(sample.time)
sample.prediction_degree = 2
sample.poly = poly(sample.time, sample.prediction_degree, raw=TRUE)
lambda.default = c(40, 41, 46, 38, 35, 34)
needed.percentage = c(.1, .03, .25, .35, .25, .12)

groups.names <- c("Не класиф.", "Змішаний.", "Слабкий",
                  "Неврів.", "Рухливий", "Інертний")
groups.number <- length(groups.names)

global.groups = rep(0, groups.number)
global.percentage = rep(0, groups.number)
global.distances = rep(0, groups.number)

result = rep(-1, n)
plots <- list()

for (j in 1:(rows*columns)) {
    for (i in 1:n) {
        ## Generate non-homogeneous Poisson process trajectory
        sample.current <- generate_trajectory(lambda.default)
        # Calculate approximation
        abc <- find_abc(sample.current)
        # Get group
        result[i] <- get_group(abc[1], abc[2], abc[3])
    }
    elements_in_group <- function(x) length(result[result==x])
    groups <- unlist(Map(elements_in_group, -1:(groups.number-2)))
    print(sprintf("%.2f", groups/n))
    print(sprintf("%.2f", chi_squared.distance(n, tail(groups/n, 5), tail(needed.percentage,5))))
    global.groups <- global.groups + groups

    info <- data.frame(group=groups.names[result+2], stringsAsFactors=FALSE)
    plots[[j]] <- ggplot(info, aes(group)) +
        geom_histogram(aes(fill=..count..)) + theme_bw() +
        theme(legend.position="none") +
        labs(x="Тип", y="Кількість студентів") +
        scale_fill_gradient("Кількість")
}

png('types.png', width=40, height=20, units="cm", res=300)
print(multiplot(plotlist=plots, cols=3))
dev.off()

print('Result:')
global.percentage = global.groups / (n*rows*columns)
print(sprintf("%.2f", global.percentage))
print(sprintf("%.2f", chi_squared.distance(n, tail(global.percentage, 5), tail(needed.percentage,5))))

variables.names <- c("NotClassified", "Neutral", "Weak", "Unstable",
                     "Movable", "Inert")
file.conn <- file("poisson_types.tex")
file.out <- rep(NA, groups.number)
for (i in 1:groups.number) {
    file.out[i] <- sprintf("\\def\\poisson%s{%.1f}", variables.names[i], global.percentage[i]*100)
}
writeLines(file.out, file.conn)
close(file.conn)
