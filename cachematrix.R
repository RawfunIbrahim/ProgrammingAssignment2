## makeCacheMatrix can be used to modify, extract the matrix itself (by 
## using get(), and set() function and extract and modify the inverse
## of the matrix. However, this function is complemented by cacheSolve()
## function. cacheSolve() calculates the inverse of our relevant matrix
## which enables us to extract it late through x$getinverse() function.
## Everytime set function is used cache data is reverted to null through 
## makeCacheMatrix() function.

## For this assignment, it is assumed that the matrix supplied is always
## invertible. Hence no additional test is provided to test its invertibility.


## makeCacheMatrix comprises get, set, getmean and setmean function.

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


## cacheSolve() function computes the inverse value through solve() function
## and stores it for later use. if the inverse of the function is already
## computed it returns the cache data rather than computing again.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
