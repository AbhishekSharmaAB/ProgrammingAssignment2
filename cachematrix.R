## These are two functions designed to calculate the inverse of a 
## matrix (assumption is the matrix provided will be invertible 
## square matrix). As matrix inversion is usually a costly computation,
## the functions are written to make use of caching to cache the
## inverse rather than computing it repeatedly.


## makeCacheMatrix function creates a special vector, which is a list
## containing functions to "set the value of the vector", "get the 
## value of the vector", "set the value of the matrix inverse" and 
## "get the value of the matrix inverse".

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special vector
## created with the above function. It first checks if the inverse
## is already calculated. If yes, it uses that inverse; else it 
## calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
