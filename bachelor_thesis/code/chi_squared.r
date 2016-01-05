chi_squared.distance <- function(m, h, p) {
    return(m * sum((h - p)^2 / p))
}
