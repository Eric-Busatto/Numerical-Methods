# Secant Method

secant_method <- function(f, x0, x1, tol = 1e-7, max_iter = 100) {
        # f: the function for which we want to find the root.
        # x0: the first initial guess.
        # x1: the second initial guess.
        # tol: the tolerance for convergence.
        # max_iter: the maximum number of iterations.
        
        iter <- 0
        while (iter < max_iter) {
                # Calculate the function values at the initial points x0 and x1:
                f_x0 <- f(x0)
                f_x1 <- f(x1)
                
                # Calculate the next approximation of the root:
                x2 <- x1 - (f_x1 * (x1 - x0) / (f_x1 - f_x0))
                
                # Check for convergence
                # based on the idea of
                # Cauchy sequence.
                if (abs(x2 - x1) < tol) {
                        cat("Root found at x =", x2, "after", iter, "iterations\n")
                        return(x2)
                }
                
                # Update the variables for the next iteration:
                x0 <- x1
                x1 <- x2
                iter <- iter + 1
        }
        
        # If the maximum number of iterations is reached without convergence,
        # then the function will return NA.
        cat("The method did not converge after", max_iter, "iterations\n")
        return(NA)
}

# Basic Example:
f <- \(x) x^2 - 2

secant_method(f, 1, 2)

