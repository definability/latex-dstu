multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

get.gg.students.pca <- function (pca, count) {
    info <- data.frame(sdev=pca$sdev[1:count],n=1:count)
    ggplot(info, aes(x=n,y=sdev)) +
        geom_bar(stat="identity", fill="lightblue") + theme_bw() +
        labs(x="Головна компонента", y="Середньоквадратичне відхилення")
}

get.gg.students.pc <- function (exam, rotations, pca.number) {
    info <- data.frame(value=exam$values %*% rotations[,pca.number],
                       group=exam$groups)
    ggplot(info, aes(value)) +
        geom_histogram(aes(fill=..count..)) + theme_bw() +
        labs(x=sprintf("Значення головної компоненти %d", pca.number),
             y="Кількість студентів") +
        theme(legend.position="none") +
        scale_fill_gradient("Кількість")
}

groups.names <- c("Не класифіковано", "Змішаний", "Слабкий",
                  "Невріважений", "Рухливий", "Інертний")

get.gg.students.pc.classified <- function (exam, rotation, groups) {
     pca.distribution <- Reduce(function (result, g) {
        rbind(result, data.frame(
              Значення=c(exam$values[exam$groups == g,]%*% rotation),
              Тип=groups.names[g+2], stringsAsFactors=FALSE))
    }, groups, c())
    ggplot(pca.distribution, aes(Значення, fill=Тип)) + theme_bw() +
           ylab("Щільність") +
           theme(legend.position="bottom") +
           geom_density(alpha = 0.2)
}
