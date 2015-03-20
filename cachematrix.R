## These functions will take in matrix and calculate the inverse
## The inverse will be stored in teh cache so that it does not have to be calculated each time
##
## Author: Aaron Berlin
## Date: March 20, 1015
## Version: 1

## Write a short comment describing this function
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        # set inverted matrix
        setmatrix <- function(inv) m <<- inv
        # get inverted matrix
        getmatrix <- function() m
        # create and return special object with cached matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function inverts the special matrix generated in makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        ## If there is cached data return that
        if(!is.null(m)) {
                message("Getting cached matrix")
                return(m)
        }
        ## If no cached data
        data <- x$get()
        ## Invert the matrix and store the inverted matrix in the cache
        m <- inv(data, ...)
        x$setmatrix(m)
        m
}

