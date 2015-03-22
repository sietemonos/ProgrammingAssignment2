## The script caches the inverse of a matrix and retrieves it
## if the content of the matrix did not change, skipping new computations.
## 
## To start, create an original matrix with makeCacheMatrix() function
## Example: 
##     v <- c(2, 3, 5, 0, 0, 1, 1, 0, 1)
##     x <- matrix(v, nrow = 3)
##     mat <- makeCacheMatrix(x)
##
## Use cacheSolve() to recover the calculated matrix inverse from cache

makeCacheMatrix <- function(x = matrix()) {
    # Creates a "special" matrix object that can cache its inverse
    #
    # Args:
    #     x: matrix object to calculate its inverse
    #
    # Returns:
    #     A list with the outcome of all sub-functions
    
    inv.matrix <- NULL  #Matrix object of the inverse matrix
    
    setMtrx <- function(x.Matrix) {
        # Sets the matrix object to be read by cacheSolve() function
        #
        # Args:
        #     y: Matrix object
        #
        # Returns:
        #     Matrix obtect 'x' to calculate its inverse and storage it
        #     into the 'inv.matrix' cache variable
        
        x <<- x.Matrix
        inv.matrix <<- NULL
        environment(inv.matrix)
    }
    
    getMtrx <- function() {
        # Returns the matrix object 'x' to computing its inverse.
        x
    }
    
    setInverse <- function(solveResult.matrix) {
        # Stores the inverse matrix that results from solve() function
        # into the 'inv.matrix' cache variable
        #
        # Args:
        #     solveResult.matrix: Matrix object result from solve() function
        # 
        # Returns the 'inv.matrix' cache variable 
        inv.matrix <<- solveResult.matrix
    }
        
    getInverse <- function() {
        # Returns the inverse matrix stored in the 'inv.matrix' cache variable
        inv.matrix
    }
    
    list(setMtrx <- setMtrx, getMtrx <- getMtrx, 
         setInverse <- setInverse, getInverse <- getInverse)
}

cacheSolve <- function(x, ...) {
    # Returns the inverse of a matrix if its stored in the cache and if the
    # matrix has not changed; otherwise, calculates the inverse of a matrix.
    # 
    # Args:
    #     x: matrix object to calculate its inverse
    #
    # Returns:
    #     The inverse matrix stored into the variable cache 'inv.matrix'
    #     or an error message.
    print("Opa")
    inv.matrix <- x$getInverse()

    if (!is.null(inv.matrix)) {
        # Return the inverse matrix of 'x' stored in cache
        message("Getting chache data")
        return(inv.matrix)
    } else {
        # Return a matrix that is the inverse of 'x'
        message("Computing a new inverse matrix")
        original.matrix <- x$getMtrx()
        inv.matrix <- solve(original.matrix, ...)
        x$setInverse(inv.matrix)
        inv.matrix
    } 
    
# Code style based on cran.r-project RStyle: 
# http://cran.r-project.org/web/packages/rockchalk/vignettes/Rstyle.pdf  
# and Google R's Style Guide: 
# https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
}