find_extrema <- function(model, time) {
    a = model$coefficients[3]
    b = model$coefficients[2]
    c = model$coefficients[1]

    t.start        = head(time, n=1)
    t.finish       = tail(time, n=1)

    y <- function(t) a*(t^2) + b*t + c
    first_derivative  <- function(t) 2*a*t   + b
    second_derivative <- function(t) 2*a

    global_extremum   <- - b / (2*a)

    initial_extremum  = global_extremum
    if (initial_extremum < t.start) {
        initial_extremum <- t.start;
    }
    else if (initial_extremum > t.finish) {
        initial_extremum <- t.finish;
    }

    t.min <- Reduce(function(x, n) ifelse(y(x) < y(n), x, n),
                    c(t.start, t.finish), initial_extremum);
    t.max <- Reduce(function(x, n) ifelse(y(x) > y(n), x, n),
                    c(t.start, t.finish), initial_extremum);
    list(min=t.min, max=t.max)
}

get_group <- function(model, time, t_min, t_max) {
    a = model$coefficients[3]
    b = model$coefficients[2]
    c = model$coefficients[1]
    e = 1
    y <- function(t) a*(t^2) + b*t + c

    if (y(t_max) - y(time[1]) <= e && y(time[1]) - y(t_min) <= e) {
        return(0)
    }
    else if(abs(t_max - time[1]) < 1E-5 && y(t_max) - y(t_min) > e) {
        return(1)
    }
    else if(time[2] <= t_max && t_max <= time[3] && time[3] < t_min
            && y(t_max) - y(time[1]) > e && y(time[1]) - y(t_min) > e) {
        return(2)
    }
    else if(time[3] <= t_max && y(t_max) - y(time[1]) > e
            && y(time[6]) <= y(time[1])) {
        return(3)
    }
    return(-1)
}
