## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## The project includes functions that can create and deal with a special 
## "matrix" object that can cache its inverse

## This function create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
        im <- NULL
        set <- function(x) {
                m <<- x
                im <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) im <<- inverse
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the function should retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
