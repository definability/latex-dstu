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

png('pca.png', width=10, height=8, units="cm", res=300)
print(get.gg.students.pca(students.pca, 5))
dev.off()

png('pca_hists.png', width=16, height=12, units="cm", res=300)
print(multiplot(get.gg.students.pc(exam, students.pca$rotation, 1),
                get.gg.students.pc(exam, students.pca$rotation, 2),
                get.gg.students.pc(exam, students.pca$rotation, 3),
                get.gg.students.pc(exam, students.pca$rotation, 4),
                cols=2))
dev.off()

png('pca_hists_detailed.png', width=30, height=22, units="cm", res=300)
print(multiplot(
      get.gg.students.pc.classified(exam, students.pca$rotation[,1], groups),
      get.gg.students.pc.classified(exam, students.pca$rotation[,2], groups),
      get.gg.students.pc.classified(exam, students.pca$rotation[,3], groups),
      get.gg.students.pc.classified(exam, students.pca$rotation[,4], groups),
      cols=2))
dev.off()

png('pca1_hist.png', width=16, height=20, units="cm", res=300)
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

print(students.cart)
print(summary(students.cart))

test.exams.number <- 4
test.exams <- Map(exam.generate, rep(600, test.exams.number))

png('pca_hists_test.png', width=30, height=22, units="cm", res=300)
test.exams.gg <- list()
for (i in 1:test.exams.number) {
    test.exams.gg[[i]] <- get.gg.students.pc.classified(test.exams[[i]],
                          students.pca$rotation[,1], groups.investigated)
}
print(multiplot(plotlist=test.exams.gg, cols=2))
dev.off()

# FILE
file.conn <- file("pca.tex")

first.component.proportion <- sprintf("\\def\\firstComponentProportion{%.2f}",
               get.pc.proportion(students.pca$sdev, 1)*100)

prediction.quality <- get.prediction.quality(students.cart, exam)

thinkers.prediction.quality <- sprintf("\\def\\thinkersPredictionQuality{%d}",
                                       round(prediction.quality$think*100))
learners.prediction.quality <- sprintf("\\def\\learnersPredictionQuality{%d}",
                                       round(prediction.quality$learn*100))

test.thinkers.prediction.quality <- rep(NA, test.exams.number)
test.learners.prediction.quality <- rep(NA, test.exams.number)
for (i in 1:test.exams.number) {
    prediction.quality <- get.prediction.quality(get.fit(
                          get.students(
                                  test.exams[[i]], students.pca$rotation[,1])),
                                  test.exams[[i]])
    test.thinkers.prediction.quality[i] <- round(prediction.quality$think*100)
    test.learners.prediction.quality[i] <- round(prediction.quality$learn*100)
}
test.thinkers.prediction.quality <- paste(sprintf("%d\\%%", test.thinkers.prediction.quality), collapse=", ")
test.thinkers.prediction.quality <- paste("\\def\\testThinkersPredictionQuality{", test.thinkers.prediction.quality, "}", sep="")
test.learners.prediction.quality <- paste(sprintf("%d\\%%", test.learners.prediction.quality), collapse=", ")
test.learners.prediction.quality <- paste("\\def\\testLearnersPredictionQuality{", test.learners.prediction.quality, "}", sep="")

writeLines(c(first.component.proportion,
             thinkers.prediction.quality, learners.prediction.quality,
             test.thinkers.prediction.quality,
             test.learners.prediction.quality), file.conn)
close(file.conn)
