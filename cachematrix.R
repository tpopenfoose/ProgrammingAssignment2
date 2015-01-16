## Functions for cacheing the matrix inverse
## toby popenfoose 2015.01.16

## makeCacheMatrix function to set up the functions and
##    storage for the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL    ## initialize the inverse to signal inverse has not been calculated yet
    set <- function(newMatrix) {
        x <<- newMatrix
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(newInverse) inverse <<- newInverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function to check for precalculated matrix inverse and
##    store calculated inverse in cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)  ## calculate the inverse of the matrix
    x$setInverse(inverse)
    inverse
}
