t.stretch.start  <- 0
t.stretch.finish <- 30

parabola.stretch <- function (a, b, c, t.finish.new) {
    t.k <- t.stretch.finish/t.finish.new
    function (t) {
        x <- t.k * t
        a*x^2+b*x+c
    }
}
