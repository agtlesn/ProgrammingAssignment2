## The functions, when computing the inverse of the matrix, 
## save the obtained result to avoid repeating 
## the time-consuming calculations for the same matrix




## The function makeCacheMatrix creates a special "matrix", 
## which contains functions to 
## set the value of the matrix, 
## get the value of the matrix, 
## set the value of its inverse using setinv, 
## get the value of its inverse using getinv.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## The function cacheSolve calculates the inverse of the special "matrix" 
## created with the function makeCacheMatrix. 
## It first checks if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix 
## and sets the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


