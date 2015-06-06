X    <- seq(5, 30, 5)
X2   <- X^2
X3   <- X^3
X4   <- X^4
N    <- 6

SX   <- sum(X)
SX2  <- sum(X2)
SX3  <- sum(X3)
SX4  <- sum(X4)
SN   <- N

MX <- matrix(c(SX2, SX3, SX4, SX, SX2, SX3, SN, SX, SX2), nrow=3, ncol=3)

find_abc <- function(sample) {
    Y   <- sample
    YX  <- Y*X
    YX2 <- Y*X2

    SY   <- sum(Y)
    SYX  <- sum(YX)
    SYX2 <- sum(YX2)

    delta <- SX2 * (SX2^2 - SX * SX3) - SX * (SX3 * SX2 - SX * SX4) + SN * (SX3^2 - SX2 * SX4)
    delta.a <- SY * (SX2 *SX2 -SX *SX3 ) - SYX * (SX *SX2 -SN *SX3 ) + SYX2 * (SX *SX -SN *SX2 )
    delta.b <- - SY * (SX3 *SX2 -SX *SX4 ) + SYX * (SX2 *SX2 -SN *SX4 ) - SYX2 * (SX2 *SX -SN *SX3 )
    delta.c <- SY * (SX3 *SX3 -SX2 *SX4 ) - SYX * (SX2 *SX3 -SX *SX4 ) + SYX2 * (SX2 *SX2 -SX *SX3 )

    abc <- c(delta.a, delta.b, delta.c)/delta
    return(abc)
}

get_group <- function(a, b, c, time) {
    d1 <- (2*a*time[1] + b)
    d6 <- (2*a*time[6] + b)
    M <- sign(a)
    V <- sign(d1*d6)
    t.g <- 0
    sigma.s <- 0
    sigma.f <- 0
    sigma.m <- 0
    sigma.p <- 0
    if (a != 0) {
        t.g <- - b / (2*a)
        sigma.s <- abs((t.g-time[1]) * d1)/2
        sigma.f <- abs((t.g-time[6]) * d6)/2
        sigma.m <- max(sigma.s, sigma.f)
        sigma.p <- sigma.m - min(sigma.s, sigma.f)
    }
    EPSILON <- 2
    if ((V == -1 && sigma.m < EPSILON) ||
        (V == 1 && sigma.p < EPSILON)) {
        return(0)
    } else if (V == 1 && sigma.p > EPSILON && (
               (t.g < time[1] && M == -1) ||
               (t.g > time[6] && M == 1)  ||
               (a == 0 && b < 0))) {
        return(1)
    } else if (M == -1 && V == -1 && t.g < time[3] &&
               sigma.f > EPSILON) {
        return(2)
    } else if (M == -1 && V == -1 && t.g >= time[3] &&
               sigma.f - sigma.s > 0) {
        return(3)
    } else if (M == 1 && V == -1 && (sigma.s - sigma.f >= -EPSILON)) {
        return(4)
    } else {
        return(-1)
    }
}
