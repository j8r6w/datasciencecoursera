## The following functions cache the inverse of a matrix so that it
## can be calculated and stored, rather than having to be calculated
## each time it is needed, which can be costly and time-intensive.

## The first function below: makeCacheMatrix, creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
              x <<- y
              s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
              setinv = setinv, 
              getinv = getinv)
}


## The following function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}