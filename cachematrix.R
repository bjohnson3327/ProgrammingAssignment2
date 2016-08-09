## These functions combine to take a user-defined matrix object (x) and create
## a cached version of the inverse (n) which can be either set if it doesn't 
## already exist (n = NULL) or can retrieve the cached value if n != NULL.

## This function takes matrix object (x) and creates a list of functions to
## either set/get the original matrix or to set/get the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()){
        n <- NULL
        setMatrix  <- function(y){
                x <<- y
                n <<- NULL
        }
        getMatrix  <- function() x
        setInverse <- function(solve) n <<- solve
        getInverse <- function() n
        list(setMatrix = setMatrix, getMatrix = getMatrix,
                setInverse = setInverse, getInverse = getInverse)
}

## This function selects the appropriate action from the list in the above
## makeCacheMatrix object returns the cached value if it exists (n != NULL)
## or uses the solve function to create the inverse matrix if it doesn't.

cacheSolve <- function(x, ...){
        n  <- x$getInverse()
        if(!is.null(n)){
                message("Getting Cached Data")
                return(n)
        }
        data <- x$getMatrix()
        n    <- solve(data)
        x$setInverse(n)
        return(n)
}
