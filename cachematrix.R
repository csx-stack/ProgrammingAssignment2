## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {     ## This function creates a special "matrix" object that can cache its inverse.

    m <- NULL
    set <- function(y) {        ## set the value of the vector
        x <<- y
        m <<- NULL
    }
    get <- function() x         ## get the value of the vector
    setinverse <- function(inverse) m <<- inverse       ## set the value of the inverse
    getinverse <- function() m                          ## get the value of the inverse
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
            ##If so, it gets the inverse from the cache and skips the computation. 
                
## Write a short comment describing this function

cacheSolve <- function(x, ...) {        ##this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
    m <- x$getinverse()
    if (!is.null(m)) {                  ##First, it checks to see if the inverse has already been calculated.                                
        message("getting cached data")  ##If so, it gets the mean from the cache and skips the computation.
        return(m)                       ##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.    
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
