# Newton's Method

newtons_method <- function(f, f_prime, x0, tol = 1e-7, max_iter = 1000) {
        x <- x0
        for (i in 1:max_iter) {
                fx <- f(x)
                fpx <- f_prime(x)
                
                if (abs(fpx) < .Machine$double.eps) {
                        stop("Derivative is too small; Newton's method fails.")
                }
                
                x_new <- x - (fx / fpx)
                
                if (abs(x_new - x) < tol) {
                        return(x_new)
                }
                
                x <- x_new
        }
        
        # SE O LOOP TERMINOU COM abs(x_new - x) > tol ENTÃO NÃO HOUVE CONVERGÊNCIA.
        
        stop("Maximum number of iterations reached without convergence.")
}

# Example:
f <- function(x){x^2 - 2}
f_prime <- function(x){2*x}

# Chute inicial:
x0 <- 1

# Find the root
root <- newtons_method(f, f_prime, x0)
cat("The root is:", root, "\n")

# EXEMPLO NO QUAL O CHUTE INICIAL ESTÁ LONGE DA RAIZ
# E O NÚMERO MÁXIMO DE ITERAÇÕES É PEQUENO.

g <- function(x){x^2 - 6*x}
g_prime <- function(x){2*x - 6}
x_0 <- 100000

newtons_method(f = g, f_prime = g_prime, x0 = x_0, tol = 1e-3, max_iter = 15)
