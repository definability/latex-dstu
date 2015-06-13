library('ggplot2')
library('party')

source("generate_sequence.r")
source("analyze_approximation.r")
source("modify_parabola.r")

source("exam_generator.r")
source("exam_analyzer.r")
source("exam_draw.r")

options(warn=1)

get.group <- function (sample, group.number) {
    sample$values[sample$groups == group.number,]
}

groups.names <- c("Не класиф.", "Зміш.", "Слабкий",
                  "Неврів.", "Рухливий", "Інерт.")

get.chart <- function(sample, i) {
    get.chart.g(sample, i, (seq(length(groups.names))-2))
}

get.amount <- function(sample, pc, group.number, f) {
    fpc <- get.group(sample, group.number) %*% students.pca$rotation[,pc]
    fpc[f(fpc)]
}

exam <- exam.generate(600)

students.pca <- prcomp(exam$values, center = TRUE)

png('pca.png', width=10, height=10, units="cm", res=300)
print(get.gg.students.pca(students.pca, 5))
dev.off()

png('pca_hists.png', width=22, height=22, units="cm", res=300)
print(multiplot(get.gg.students.pc(exam, students.pca$rotation, 1),
                get.gg.students.pc(exam, students.pca$rotation, 2),
                get.gg.students.pc(exam, students.pca$rotation, 3),
                get.gg.students.pc(exam, students.pca$rotation, 4),
                cols=2))
dev.off()

png('pca_hists_detailed.png', width=22, height=22, units="cm", res=300)
print(multiplot(
      get.gg.students.pc.classified(exam, students.pca$rotation[,1], groups),
      get.gg.students.pc.classified(exam, students.pca$rotation[,2], groups),
      get.gg.students.pc.classified(exam, students.pca$rotation[,3], groups),
      get.gg.students.pc.classified(exam, students.pca$rotation[,4], groups),
      cols=2))
dev.off()

png('pca1_hist.png', width=22, height=22, units="cm", res=300)
print(multiplot(
      get.gg.students.pc.classified(exam, students.pca$rotation[,1], groups),
      get.gg.students.pc.classified(exam, students.pca$rotation[,1], groups.investigated),
      cols=1))
dev.off()

students.training <- get.students(exam, students.pca$rotation[,1])
students.cart <- get.fit(students.training)
png('tree.png', width=12, height=18, units="cm", res=300)
plot(students.cart, main="")
dev.off()

#treeresponse(fit.good, newdata=students[1:10,])
