## Put comments here that give an overall description of what your
## functions do

# Function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    getInverse <- function() {
        inv
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Function to compute the inverse of the special matrix and cache it

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
