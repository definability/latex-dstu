generate_trajectory <- function(lambda.default) {
    # Generate non-homogeneous Poisson process
    return(rpois(length(lambda.default)-1, lambda.default))
}
