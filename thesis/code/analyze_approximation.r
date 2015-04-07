find_extrema <- function(model, time) {
    a = model$coefficients[3]
    b = model$coefficients[2]
    c = model$coefficients[1]

    start_time        = head(time, n=1)
    finish_time       = tail(time, n=1)

    original_function <- function(t) a*(t^2) + b*t + c
    first_derivative  <- function(t) 2*a*t   + b
    second_derivative <- function(t) 2*a

    global_extremum   <- - b / (2*a)

    initial_extremum  = global_extremum
    if (initial_extremum < start_time) {
        initial_extremum <- start_time;
    }
    else if (initial_extremum > finish_time) {
        initial_extremum <- finish_time;
    }

    t.min <- Reduce(function(x, y) ifelse(original_function(x) < original_function(y), x, y), c(start_time, finish_time), initial_extremum);
    t.max <- Reduce(function(x, y) ifelse(original_function(x) > original_function(y), x, y), c(start_time, finish_time), initial_extremum);
    return(c(t.min, t.max))
}
