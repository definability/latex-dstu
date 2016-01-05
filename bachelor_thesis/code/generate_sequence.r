generate_trajectory <- function(lambda.default) {
    # Generate non-homogeneous Poisson process
    return(rpois(length(lambda.default), lambda.default))
}
