library(NHPoisson)

wideScreen();

n <- 3;
m <- 3;
png('mygraph.png', width=18, height=22, units="cm", res=80);
#png('mygraph.png');
par(mfrow=c(n,m));
sample.size = 6;
lambda.weak = 1:6;
lambda.avrg = rep(30,sample.size+1);
#lambda.default = c(43, 40, 38, 37, 38, 35)
#lambda.default = c(40, 40, 40, 40, 40, 40);
lambda.default = c(40, 38, 36, 34, 32, 30);
for (i in 1:(n*m)) {
    start_rhythm <- sample(35:45, 1)
    aux <- simNHP.fun(lambda=lambda.avrg);
    noise.max = 2;
    noise.min = 0;
    noise.mean = (noise.max + noise.min)/2
    #noise <- rbinom(sample.size,noise.max,0.5) - noise.mean;
    #cur <- unlist((Map(function(x) start_rhythm - length(aux$posNH[aux$posNH==x]), 0:(sample.size - 1)))) + noise;
    #cur <- unlist((Map(function(x) length(aux$posNH[aux$posNH==x]), 1:sample.size))) + noise;
    cur <- unlist((Map(function(x) length(aux$posNH[aux$posNH==x]), 1:sample.size)));
    #plot(c(cur), xlab="Time", ylab="Taps", ylim=c(start_rhythm-10, start_rhythm+10));
    #cur = cur/cur[1] - 1;
    #plot(c(cur), xlab="Time", ylab="Taps", ylim=c(-.5,.5));
    plot(c(cur), xlab="Час", ylab="Кількість постукувань", ylim=c(20,40));
    #abline(lm(c(cur) ~ poly(c(1,2,3,4,5,6), 2, raw=TRUE)));
    time  <- c(1,2,3,4,5,6);
    model <- lm(c(cur) ~ poly(time, 2, raw=TRUE));
    predictedcounts <- predict(model);
    lines(time, predictedcounts, col='red');
    #cur = cur/start_rhythm - 1;
    #plot(c(cur), xlab="Time", ylab="Taps", ylim=c(-0.2, 0.2));
    lines(c(cur));
    print('----------NEW MODEL----------');
    print(summary(lm(c(cur) ~ poly(c(1,2,3,4,5,6), 2, raw=TRUE))));
    print('----------END----------');
}
dev.off();
