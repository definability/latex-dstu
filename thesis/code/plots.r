library(NHPoisson)

n <- 2;
m <- 2;
png('mygraph.png', width=18, height=18, units="cm", res=100);
par(mfrow=c(n,m));
sample.size = 30;
lambda.weak = 1:30/5;
lambda.avrg = rep(2,sample.size);
for (i in 1:(n*m)) {
    start_rhythm <- sample(7:9, 1)
    aux <- simNHP.fun(lambda=lambda.weak);
    noise.max = 0;
    noise.min = 0;
    noise.mean = (noise.max + noise.min)/2
    noise <- rbinom(sample.size,noise.max,0.5) - noise.mean;
    cur <- unlist((Map(function(x) start_rhythm - length(aux$posNH[aux$posNH==x]), 0:(sample.size - 1)))) + noise;
    #plot(c(cur), xlab="Time", ylab="Taps", ylim=c(start_rhythm-10, start_rhythm+10));
    plot(c(cur), xlab="Time", ylab="Taps");
    #cur = cur/start_rhythm - 1;
    #plot(c(cur), xlab="Time", ylab="Taps", ylim=c(-0.2, 0.2));
    lines(c(cur));
}
dev.off();
