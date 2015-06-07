t.start <- 5
t.middle <- 15
t.finish <- 30

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

delta <- SX2 * (SX2^2 - SX * SX3) - SX * (SX3 * SX2 - SX * SX4) +
         SN * (SX3^2 - SX2 * SX4)

delta.a.k <- c(SX2^2 -SX * SX3, -(SX * SX2 - SN * SX3), SX * SX - SN * SX2)/delta

delta.b.k <- c(-(SX3 * SX2 -SX * SX4), SX2^2 - SN * SX4, -(SX2 * SX - SN * SX3))/delta

delta.c.k <- c(SX3^2 - SX2 * SX4, -(SX2 * SX3 - SX * SX4), SX2 * SX2 - SX * SX3)/delta

MX <- matrix(c(SX2, SX3, SX4, SX, SX2, SX3, SN, SX, SX2), nrow=3, ncol=3)

find_abc <- function(sample) {
    SY.k <- c(sum(sample), sample %*% X, sample %*% X2)
    return(c(SY.k %*% delta.a.k, SY.k %*% delta.b.k, SY.k %*% delta.c.k))
}

get_group <- function(a, b, c) {
    d1 <- (2*a*t.start + b)
    d6 <- (2*a*t.finish + b)
    M <- sign(a)
    V <- sign(d1*d6)
    t.g <- 0
    sigma.s <- 0
    sigma.f <- 0
    sigma.m <- 0
    sigma.p <- 0
    if (a != 0) {
        t.g <- - b / (2*a)
        sigma.s <- abs((t.g-t.start) * d1)/2
        sigma.f <- abs((t.g-t.finish) * d6)/2
        sigma.m <- max(sigma.s, sigma.f)
        sigma.p <- sigma.m - min(sigma.s, sigma.f)
    }
    EPSILON <- 2
    if ((V == -1 && sigma.m < EPSILON) ||
        (V == 1 && sigma.p < EPSILON)) {
        return(0)
    } else if (V == 1 && sigma.p > EPSILON && (
               (t.g < t.start && M == -1) ||
               (t.g > t.finish && M == 1)  ||
               (a == 0 && b < 0))) {
        return(1)
    } else if (M == -1 && V == -1 && t.g < t.middle &&
               sigma.f > EPSILON) {
        return(2)
    } else if (M == -1 && V == -1 && t.g >= t.middle &&
               sigma.f - sigma.s > 0) {
        return(3)
    } else if (M == 1 && V == -1 && (sigma.s - sigma.f >= -EPSILON)) {
        return(4)
    } else {
        return(-1)
    }
}
