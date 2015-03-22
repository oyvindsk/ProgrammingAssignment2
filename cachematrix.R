
## This file is the solution to programming assignment 2 and contains 2 functions.

## Make a list of functions that work on a captured varible to store a matrix and a cached version of the inverse of the same matrix. 
## No computation in this funxtion, only getter & setter functions that work on the captured variable.

makeCacheMatrix <- function(x = matrix()) {

    ## Store the cached inverse matrix "inside" this closure
    cache <- NULL
    
    ## Getter / setter functions to return
    get         <- function()  x
    getInverse  <- function()  cache
    setInverse  <- function(m) cache <<- m 
    
    ## return a list of functions, like the example code
    list(get = get, getInverse = getInverse, setInverse = setInverse)
}


## Solve the inverse of a matrix, using the closue we created with makeCacheMatrix
## If the cache is set return that. If not, solve() for matrix and return the result after setting the cache

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse() # # the varibale we will return after setting it to the correct matrix
    
    if(is.null(inv)){
        # Nothing cached, solve and fill the cache
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInverse(inv)
    } else {
        # Got cached result, nothing to do
        message("getting cached data")
    }
    
    inv

}
