# The Bisection Method

bisection <- function(f, a, b, tol = 1e-7, max.iter = 1000) {
        # Check the function values at the endpoints:
        if (f(a) * f(b) > 0) {
                stop("Function values at the endpoints must have opposite signs.")
        }
        
        iter <- 0
        while ((b - a) / 2 > tol && iter < max.iter) {
                iter <- iter + 1
                c <- (a + b) / 2
                if (f(c) == 0 || (b - a) / 2 < tol) {
                        return(list(root = c, iterations = iter))
                }
                if (f(c) * f(a) < 0) {
                        b <- c
                } else {
                        a <- c
                }
        }
        
        return(list(root = (a + b) / 2, iterations = iter))
}

# Example:
f <- function(x) { x^2 - 2 }
result <- bisection(f, 0, 2)
cat("Root:", result$root, "found in", result$iterations, "iterations\n")

h <- function(x){x - 0.2*sin(x) - 0.5}
