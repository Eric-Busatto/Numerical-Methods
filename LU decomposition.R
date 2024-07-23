# LU decomposition

matrix_LU <- function(a) {
        # n rows for matrix
        n <- nrow(a)
        # create upper and lower matrix
        U <- matrix(0, ncol = n, nrow = n) # same size as a all zeros
        # lower has to have 1's on diagonal
        L <- diag(x = 1, ncol = n, nrow = n) # identity matrix the size of a
        
        # loop through each row
        for (i in 1:n) {
                # create variables for i+1 and i-1
                i_p_1 <- i + 1
                i_m_1 <- i - 1
                for (j in 1:n) {
                        # loop through matrix and copy a to U
                        U[i,j] <- a[i,j]
                        # if i-1 more than zero
                        if (i_m_1 > 0) {
                                # solve U matrix equation
                                # iterate through U and L subtracting from L, U product second row
                                # we know that row 1 will be the same in U 
                                for (k in 1:i_m_1) {
                                        U[i,j] <- U[i,j] - L[i,k] * U[k,j]
                                }
                        }
                }
                if (i_p_1 <= n) {
                        # solve L matrix equation
                        # we know that upper portion is all zeros and diagonal are 1's
                        # loop through j i+1 to n 
                        for (j in i_p_1:n) {
                                # copy lower portion of a to L
                                L[j,i] <- a[j,i]
                                # loop through rows starting at 2nd row
                                if (i_m_1 > 0) {
                                        # solve lower half of matrix
                                        # for k in i-1 (starts on 2nd row)
                                        for (k in 1:i_m_1) {
                                                L[j,i] <- L[j,i] - L[j,k] * U[k,i]
                                        }
                                }
                                # divide portion of equation
                                L[j,i] <- L[j,i] / U[i,i]
                        }
                } 
        }
        
        result <- list(L = L, U = U)
        
        return(result)
}

# Example:

A <- matrix(c(2, 0, 1, 4, 3, 7, 6, 6, 16), nrow = 3, byrow = TRUE)

lu_result <- matrix_LU(A)
L <- lu_result$L
U <- lu_result$U

print("L matrix:")
print(L)
print("U matrix:")
print(U)
