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
