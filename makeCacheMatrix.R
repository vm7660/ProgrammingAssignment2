## create the special function for caching the inverse of squre matrices
makeCacheMatrix <- function(x = matrix()) {
	if( ! is.matrix(x) ){
    	    stop("Input is expected to be a matrix")
	}
	if( nrow(x) != ncol(x) ){
    	    stop("Input is expected to be a square matrix")
	}
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## function to that attempts to fetch the value from cache if available
cacheSolve <- function(x, ...) {
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}

# UnitTest Results
##> c1 <- c(2, 4, 3, 1, 5, 7,0,8,9)
##> m1 <- makeCacheMatrix(c1)
##Error in makeCacheMatrix(c1) : Input is expected to be a matrix
##> B1 = matrix(c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2)
##> m1 <- makeCacheMatrix(B1)
##Error in makeCacheMatrix(B1) : Input is expected to be a square matrix
##> B = matrix(c(2, 4, 3, 1, 5, 7,0,8,9), nrow=3, ncol=3)
##> solve(B)
##           [,1]       [,2]       [,3]
##[1,]  0.3235294  0.2647059 -0.2352941
##[2,]  0.3529412 -0.5294118  0.4705882
##[3,] -0.3823529  0.3235294 -0.1764706
##> class(B)
##[1] "matrix"
##> m1 <- makeCacheMatrix(B)
##> cacheSolve(m1)
##           [,1]       [,2]       [,3]
##[1,]  0.3235294  0.2647059 -0.2352941
##[2,]  0.3529412 -0.5294118  0.4705882
##[3,] -0.3823529  0.3235294 -0.1764706
##> cacheSolve(m1)
##getting cached data
##           [,1]       [,2]       [,3]
##[1,]  0.3235294  0.2647059 -0.2352941
##[2,]  0.3529412 -0.5294118  0.4705882
##[3,] -0.3823529  0.3235294 -0.1764706
##
