## 2 functions that combine to provide the inverse of a matrix
## by either calculating the inverse or returning the cached inverse
## if already calculated before and matrix has not changed


## makeCacheMatrix with input a matrix will return a list of
## functions set, get, setinverse, getinverse to be used specifically
## by cacheSolve to set or get a matrix, or set or get inverse matrix 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

##cacheSolve with input of makeCacheMatrix class to calculate the
##inverse of the matrix embedded in makeCacheMatrix if it was not
##already calculated before
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
