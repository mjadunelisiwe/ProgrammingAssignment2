## The makeCacheMatrix  creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the value of the inverse of a matrix.
## 4. get the value of the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        set <- function(y) {
                x <<- y
                invX <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) invX <<- solve(x) ## calculate the inverse of a matrix
        getInverse <- function() invX 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Check if the inverse of a matrix is already calculatd and get it from the cache. if the inverse is not calculated it then calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getInverse()
        if(!is.null(invX)) {
                message("getting cached data")
                return(invX)
        }
        data <- x$get()
        invX <- solve(data, ...)
        x$setInverse(invX)
        invX
}
