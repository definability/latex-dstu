generate_trajectory <- function(lambda.default) {
    # Generate non-homogeneous Poisson process
    sample.trajectory <- simNHP.fun(lambda=lambda.default)
    # Calculate number of events in every sample.time interval
    # (we have sample.size of them)
    events_on_step <- function(x) length(sample.trajectory[
                                         sample.trajectory$posNH==x])
    sample.current <- unlist((Map(events_on_step, 1:(sample.size))))
    return(sample.current)
}
